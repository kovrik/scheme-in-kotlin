package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.lists.Append
import core.procedures.predicates.Predicate
import core.scm.Vector
import core.Writer
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

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        2    -> quasiquote(0, form[1]!!, env, evaluator)
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

    private fun quasiquotePair(depth: Int, expr: Pair<*, *>, env: Environment, evaluator: Evaluator): Any? {
        val result = quasiquoteList(depth, expr.toList(), env, evaluator, true)
        return when (result) {
            is List<*> -> Utils.cons(result)
            else -> result
        }
    }
}
