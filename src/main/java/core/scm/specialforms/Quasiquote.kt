package core.scm.specialforms

import core.environment.Environment
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

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
object Quasiquote : SpecialForm("quasiquote") {

    private val append  = Append()
    private val first  = First()
    private val second  = Second()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
//        2    -> quasiquote(0, form[1]!!, env, evaluator)
        2 -> when (isUnquoteSplicing(form[1]) && form[1] !is Pair<*, *>) {
            true -> throw IllegalSyntaxException(UnquoteSplicing.toString(), Writer.write(form), "invalid context within quasiquote")
            false -> qq(0, form[1]!!, env, evaluator)
        }
        else -> throw IllegalSyntaxException(toString(), Writer.write(form))
    }

    private fun qq(depth: Int, expr: Any?, env: Environment, evaluator: Evaluator): Any? = when {
        expr is Vector -> {
            if (expr.isEmpty()) {
                expr
            } else if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            } else {
                qq(depth, expr.toList(), env, evaluator).let {
                    when {
                        !Predicate.isProperList(it) -> throw IllegalSyntaxException("read: illegal use of '.'")
                        else -> MutableVector(it as List<*>)
                    }
                }
            }
        }
        expr is Set<*> -> {
            if (expr.isEmpty()) {
                expr
            } else if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            } else {
                qq(depth, expr.toList(), env, evaluator).let {
                    when (it is Collection<*> && Predicate.isProperList(it)) {
                        true -> MutableSet(it)
                        false -> throw IllegalSyntaxException("read: illegal use of '.'")
                    }
                }
            }
        }
        // FIXME Nested quasiquotation  ``(,,@'() . 2)
        expr is Pair<*, *> -> when {
            isUnquote(expr) -> Pair(Unquote.symbol, qq(depth, expr.second, env, evaluator))
            isUnquoteSplicing(expr.first) && depth == 0 -> append(evaluator.eval(second(expr.first), env), qq(depth, expr.second, env, evaluator))
            else -> expr
        }
        !Predicate.isPairOrNonEmptyList(expr) -> expr
        expr is List<*> -> when {
            isQuasiquote(expr) -> when (expr.size == 2) {
                true -> listOf(symbol, qq(depth + 1, second(expr), env, evaluator))
                false -> throw IllegalSyntaxException(toString(), Writer.write(expr))
            }
            isUnquote(expr) -> when (expr.size == 2) {
                true -> when (depth == 0) {
                    true -> evaluator.eval(second(expr), env)
                    false -> listOf(Unquote.symbol, qq(depth - 1, second(expr), env, evaluator))
                }
                false -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "unquote expects exactly one expression")
            }
            else -> {
                var expanded: Any? = emptyList<Nothing>()
                loop@ for ((index, it) in expr.withIndex()) {
                    val e = qq(depth, it!!, env, evaluator)
                    when {
                        isUnquoteSplicing(it) -> expanded = when (depth == 0) {
                            true -> append(expanded, evaluator.eval(second(e), env))
                            false -> append(expanded, qq(depth - 1, second(e), env, evaluator))
                        }
                        it == Unquote.symbol && depth == 0 -> when (index == expr.size - 2) {
                            true -> {
                                expanded = append(expanded, evaluator.eval(expr.last(), env))
                                break@loop
                            }
                            false -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "expects exactly one expression")
                        }
                        else -> expanded = append(expanded, listOf(e))
                    }
                }
                expanded
            }
        }
        else -> expr
    }

    private fun isQuasiquote(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == Quasiquote.symbol

    private fun isUnquote(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == Unquote.symbol

    private fun isUnquoteSplicing(expr: Any?) = Predicate.isPairOrNonEmptyList(expr) && first(expr) == UnquoteSplicing.symbol

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
    // TODO Simplify
    private fun quasiquote(depth: Int, expr: Any, env: Environment, evaluator: Evaluator): Any? = when {
        /* Nothing to process */
        expr is Collection<*> && expr.isEmpty() -> expr
        expr is Vector -> {
            if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            }
            quasiquoteList(depth, expr.toList(), env, evaluator).let {
                when {
                    !Predicate.isProperList(it) -> throw IllegalSyntaxException("read: illegal use of '.'")
                    else -> MutableVector(it as List<*>)
                }
            }
        }
        expr is Set<*> -> {
            if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            }
            quasiquoteList(depth, expr.toList(), env, evaluator).let {
                when (it is Collection<*> && Predicate.isProperList(it)) {
                    true  -> MutableSet(it)
                    false -> throw IllegalSyntaxException("read: illegal use of '.'")
                }
            }
        }
        /* Evaluate case when Quasiquote is immediately followed by Unquote: `,(+ 1 2) => 3 */
        expr is List<*> && expr[0] == Unquote.symbol -> when (expr.size) {
            2    -> append(emptyList<Nothing>(), evaluator.eval(expr[1], env))
            else -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "unquote expects exactly one expression")
        }
        expr is List<*> -> quasiquoteList(depth, expr, env, evaluator)
        expr is Pair<*, *> -> quasiquotePair(depth, expr, env, evaluator)
        /* (quasiquote datum) => (quote datum) */
        else -> expr
    }

    // TODO Optimize and simplify
    private fun quasiquoteList(depth: Int, expr: List<*>, env: Environment, evaluator: Evaluator, pair: Boolean = false): Any? {
        var result: Any? = listOf<Any>()
        for (i in expr.indices) {
            val cur = expr[i]
            /* Append quoted forms recursively */
            if (cur is List<*> && cur.isNotEmpty()) {
                val op = cur.first()
                if (depth == 0 && (op == Unquote.symbol || op == UnquoteSplicing.symbol)) {
                    if (cur.size != 2) {
                        throw IllegalSyntaxException(op.toString(), Writer.write(expr), "expects exactly one expression")
                    }
                    /* Level of quasiquotation is 0 - evaluate! */
                    val eval = evaluator.eval(cur[1], env)
                    result = when (op == Unquote.symbol) {
                        true  -> append(result, listOf(eval))
                        false -> append(result, eval)
                    }
                } else {
                    val newDepth = when (op) {
                        Quasiquote.symbol -> depth + 1
                        UnquoteSplicing.symbol -> depth - 1
                        Unquote.symbol -> depth - 1
                        else -> depth
                    }
                    result = append(result, listOf(quasiquoteList(newDepth, cur, env, evaluator)))
                }
            } else if (depth == 0 && i > 0 && cur == Unquote.symbol) {
                /* Check special cases: `(1 unquote 2) => `(1 . 2) */
                /* if UNQUOTE is just before the last element a */
                if (i != expr.size - 2) {
                    throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "expects exactly one expression")
                }
                /* Evaluate and append last element */
                return append(result, evaluator.eval(expr[i + 1], env))
            } else if (cur == UnquoteSplicing.symbol && !pair) {
                /* `,@(list 1 2) syntax is not valid */
                throw IllegalSyntaxException(UnquoteSplicing.toString(), Writer.write(expr), "invalid context within quasiquote")
            } else {
                result = append(result, listOf(cur))
            }
        }
        return result
    }

    private fun quasiquotePair(depth: Int, expr: Pair<*, *>, env: Environment, evaluator: Evaluator) =
            when (val result = quasiquoteList(depth, expr.toList(), env, evaluator, true)) {
                is List<*> -> Utils.cons(result)
                else -> result
            }
}
