package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.lists.Append
import core.procedures.predicates.Predicate
import core.scm.Vector
import core.Writer
import core.procedures.seqs.First
import core.procedures.seqs.Second
import core.scm.MutableVector
import core.scm.MutableSet
import core.utils.Utils

import kotlin.collections.Set

/**
 * Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
object Quasiquote : SpecialForm("quasiquote") {

    private val append  = Append()
    private val first  = First()
    private val second  = Second()

    override fun eval(form: List<Any?>, evaluator: Evaluator) = when (form.size) {
        2 -> when (isUnquoteSplicing(form[1]) && form[1] !is Pair<*, *>) {
            true -> throw IllegalSyntaxException(UnquoteSplicing.toString(), Writer.write(form), "invalid context within quasiquote")
            false -> qq(0, form[1]!!, evaluator)
        }
        else -> throw IllegalSyntaxException(toString(), Writer.write(form))
    }

    /**
     * Implement Quasiquotation using Append and List:
     *
     * 1. wrap each element, except for unquote-splicing forms, in a call to LIST
     * 2. APPEND the results
     * - quoted elements get processed recursively
     * - unquoted elements are passed to the call to LIST unprocessed
     * - unquote-splicing forms are inserted directly into the APPEND form
     *
     * http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
     */
    private fun qq(depth: Int, expr: Any?, evaluator: Evaluator): Any? = when {
        Utils.isEmpty(expr) -> expr
        expr is List<*> -> when {
            isQuasiquote(expr) -> when (expr.size == 2) {
                true -> listOf(Quasiquote.symbol, qq(depth + 1, second(expr), evaluator))
                false -> throw IllegalSyntaxException(toString(), Writer.write(expr))
            }
            isUnquote(expr) -> when (expr.size == 2) {
                true -> when (depth == 0) {
                    true -> evaluator.eval(second(expr))
                    false -> when (depth == 1 && isUnquoteSplicing(second(expr))) {
                        true  -> append(listOf(Unquote.symbol), evaluator.eval(second(second(expr))))
                        false -> listOf(Unquote.symbol, qq(depth - 1, second(expr), evaluator))
                    }
                }
                false -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "unquote expects exactly one expression")
            }
            // TODO More tests for nested quasiquotation
            else -> {
                var expanded: Any? = emptyList<Nothing>()
                loop@ for ((index, it) in expr.withIndex()) {
                    when  {
                        isUnquoteSplicing(it) -> expanded = when (depth == 0) {
                            true  -> append(expanded, evaluator.eval(second(it)))
                            false -> append(expanded, qq(depth - 1, second(), evaluator))
                        }
                        it == Unquote.symbol && depth == 0 -> when (index == expr.size - 2) {
                            true -> {
                                expanded = append(expanded, evaluator.eval(expr.last()))
                                break@loop
                            }
                            false -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(it), "expects exactly one expression")
                        }
                        else -> expanded = append(expanded, listOf(qq(depth, it!!, evaluator)))
                    }
                }
                expanded
            }
        }
        expr is Vector -> {
            if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            } else {
                qq(depth, expr.toList(), evaluator).let {
                    when {
                        !Predicate.isProperList(it) -> throw IllegalSyntaxException("read: illegal use of '.'")
                        else -> MutableVector(it as List<*>)
                    }
                }
            }
        }
        expr is Set<*> -> {
            if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            } else {
                qq(depth, expr.toList(), evaluator).let {
                    when (it is Collection<*> && Predicate.isProperList(it)) {
                        true -> MutableSet(it)
                        false -> throw IllegalSyntaxException("read: illegal use of '.'")
                    }
                }
            }
        }
        expr is Pair<*, *> -> when {
            isUnquote(expr) -> when (depth == 1 && isUnquoteSplicing(expr.second)) {
                true -> Pair(Unquote.symbol, evaluator.eval(second(second(expr))))
                false -> Pair(Unquote.symbol, qq(depth, expr.second, evaluator))
            }
            isUnquoteSplicing(expr.first) && depth == 0 -> append(evaluator.eval(second(expr.first)), qq(depth, expr.second, evaluator))
            else -> Pair(qq(depth, expr.first, evaluator), qq(depth, expr.second, evaluator))
        }
        else -> expr
    }

    private fun isQuasiquote(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == Quasiquote.symbol

    private fun isUnquote(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == Unquote.symbol

    private fun isUnquoteSplicing(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == UnquoteSplicing.symbol
}
