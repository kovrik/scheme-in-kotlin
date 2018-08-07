package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.lists.Append
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.cons.ConsProc
import core.procedures.predicates.Predicate
import core.procedures.seqs.First
import core.procedures.sets.SetProc
import core.procedures.vectors.ListToVector
import core.procedures.vectors.VectorToList
import core.scm.Vector
import core.Writer
import core.scm.MutableVector

import kotlin.collections.Set

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
object Quasiquote : SpecialForm("quasiquote") {

    private val setProc      = SetProc()
    private val car          = Car()
    private val cdr          = Cdr()
    private val cons         = ConsProc()
    private val append       = Append()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        2    -> quasiquote(form[1]!!, env, evaluator)
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
    private fun quasiquote(expr: Any, env: Environment, evaluator: Evaluator): Any? = when {
        /* Nothing to process */
        expr is Collection<*> && expr.isEmpty() -> expr
        /* Evaluate case when Quasiquote is immediately followed by Unquote: `,(+ 1 2) => 3 */
        expr is List<*> && Predicate.isProperList(expr) && expr[0] == Unquote.symbol -> when (expr.size) {
            2    -> evaluator.eval(expr[1], env)
            else -> throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "unquote expects exactly one expression")
        }
        expr is Vector -> {
            if (expr.first() == Unquote.symbol || expr.first() == UnquoteSplicing.symbol) {
                throw IllegalSyntaxException(expr.first().toString(), Writer.write(expr), "invalid context within quasiquote")
            }
            quasiquoteList(0, expr.toList(), env, evaluator).let {
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
            quasiquoteList(0, expr.toList(), env, evaluator).let {
                when {
                    !Predicate.isProperList(it) -> throw IllegalSyntaxException("read: illegal use of '.'")
                    else -> setProc(it)
                }
            }
        }
        expr is List<*> -> quasiquoteList(0, expr, env, evaluator)
        /* (quasiquote datum) => (quote datum) */
        else -> expr
    }

    // TODO Optimize and simplify
    private fun quasiquoteList(depth: Int, expr: List<*>, env: Environment, evaluator: Evaluator): Any? {
        val isList = Predicate.isProperList(expr)
        var result: Any? = listOf<Any>()
        for (i in expr.indices) {
            val o = expr[i]
            /* Append quoted forms recursively */
            if (o !is List<*> || o.isEmpty()) {
                /* Check special cases: `(1 unquote 2) => `(1 . 2) */
                if (i > 0 && o == Unquote.symbol) {
                    /* if UNQUOTE is just before the last element a */
                    if (i != expr.size - 2) {
                        throw IllegalSyntaxException(Unquote.toString(), Writer.write(expr), "expects exactly one expression")
                    }
                    /* Evaluate and append last element */
                    return append(result, evaluator.eval(expr[i + 1], env))
                }
                /* `,@(list 1 2) syntax is not valid */
                if (isList && o == UnquoteSplicing.symbol) {
                    throw IllegalSyntaxException(UnquoteSplicing.toString(), Writer.write(expr), "invalid context within quasiquote")
                }
                /* Otherwise, just append the element wrapped with LIST */
                result = append(result, listOf(o))
            } else {
                val op = o[0]
                if (op == Quasiquote.symbol) {
                    /* Increase depth of quasiquotation */
                    result = append(result, listOf(quasiquoteList(depth + 1, o, env, evaluator)))
                } else if (op == Unquote.symbol || op == UnquoteSplicing.symbol) {
                    if (o.size != 2) {
                        throw IllegalSyntaxException(op.toString(), Writer.write(expr), "expects exactly one expression")
                    }
                    if (depth == 0) {
                        /* Level of quasiquotation is 0 - evaluate! */
                        val eval = evaluator.eval(o[1], env)
                        if (op == UnquoteSplicing.symbol) {
                            result = when (op) {
                                /* Unquote Splicing: splice and append elements into resulting list */
                                /* `(,@(list 1 2 3)) => `(1 2 3) */
                                UnquoteSplicing.symbol -> when (eval) {
                                    is Sequence<*>   -> (result as List<*>).plus(eval)
                                    is Collection<*> -> (result as List<*>).plus(eval)
                                    else -> eval
                                }
                                /* Unquote: append list with results */
                                /* `(,(list 1 2 3)) => `((1 2 3)) */
                                else -> append(result, listOf(eval))
                            }
                        } else {
                            /* Unquote: append list with results */
                            /* `(,(list 1 2 3)) => `((1 2 3)) */
                            result = append(result, listOf(eval))
                        }
                    } else {
                        /* Decrease depth of quasiquotation */
                        result = append(result, listOf(quasiquoteList(depth - 1, o, env, evaluator)))
                    }
                } else {
                    result = append(result, listOf(quasiquoteList(depth, o, env, evaluator)))
                }
            }
        }
        if (!isList) {
            /* In the case of a pair, if the cdr of the relevant quoted pair is empty,
             * then expr need not produce a list, and its result is used directly in place of the quoted pair */
            if (!Predicate.isPair(cdr(result))) {
                return (result as List<*>)[0]
            } else {
                // TODO Is car(cdr(result)) correct?
                return cons(car(result), car(cdr(result)))
            }
        }
        return result
    }
}
