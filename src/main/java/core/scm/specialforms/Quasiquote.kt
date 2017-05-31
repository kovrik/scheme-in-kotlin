package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.cons.Append
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.procedures.cons.ConsProc.Companion.cons
import core.procedures.generic.First
import core.procedures.sets.SetProc
import core.procedures.vectors.ListToVector
import core.procedures.vectors.VectorToList
import core.scm.Cons
import core.scm.Cons.Companion.EMPTY
import core.scm.Cons.Companion.isList
import core.scm.Cons.Companion.isNull
import core.scm.Cons.Companion.list
import core.scm.MutableVector

import core.scm.Symbol
import core.scm.specialforms.Unquote.UNQUOTE
import core.scm.specialforms.UnquoteSplicing.UNQUOTE_SPLICING

import kotlin.collections.Set

/* Syntax:
 * (quasiquote <datum>)
 * `<datum>
 */
enum class Quasiquote : ISpecialForm {
    QUASIQUOTE;

    companion object {
        val QUASIQUOTE_SYMBOL: Symbol = Symbol.intern(QUASIQUOTE.toString())
    }

    private val setProc = SetProc()

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (expression.size != 2) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        return quasiquote(expression[1]!!, env, evaluator)
    }

    /**
     * Implement Quasiquotation using Append and List:

     * 1. wrap each element, except for unquote-splicing forms, in a call to LIST
     * 2. APPEND the results
     * - quoted elements get processed recursively
     * - unquoted elements are passed to the call to LIST unprocessed
     * - unquote-splicing forms are inserted directly into the APPEND form

     * http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
     */
    // TODO Simplify
    private fun quasiquote(expr: Any, env: Environment, evaluator: Evaluator): Any? {
        if (expr is MutableVector) {
            /* Vector quasiquotation */
            return quasiquoteVector(expr, env, evaluator)
        } else if (expr is Set<*>) {
            /* Set quasiquotation */
            return quasiquoteSet(expr, env, evaluator)
        } else if (expr is List<*>) {
            val list = expr
            if (list.isEmpty()) {
                /* Nothing to process */
                return list
            }
            /* Evaluate case when Quasiquote is immediately followed by Unquote: `,(+ 1 2) => 3 */
            if (isList(list) && Unquote.UNQUOTE_SYMBOL == list[0]) {
                if (list.size != 2) {
                    throw IllegalSyntaxException.of(UNQUOTE.toString(), expr, "unquote expects exactly one expression")
                }
                return evaluator.eval(list[1], env)
            }
            /* `,@(list 1 2) syntax is not valid */
            if (isList(list) && list.size > 0 && UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == list[0]) {
                throw IllegalSyntaxException.of(list[0].toString(), expr, "invalid context within quasiquote")
            }
            /* List quasiquotation */
            return quasiquoteList(0, expr, env, evaluator)
        }
        /* (quasiquote datum) => (quote datum) */
        return expr
    }

    // TODO Optimize and simplify
    private fun quasiquoteList(depth: Int, expr: Any, env: Environment, evaluator: Evaluator): Any? {
        val list = expr as List<*>
        val isList = isList(list)
        var result: Any? = list<Any>()
        for (n in list.indices) {
            val o = list[n]
            /* Append quoted forms recursively */
            if (o !is List<*> || EMPTY == o) {
                /* Check special cases: `(1 unquote 2) => `(1 . 2) */
                if (n > 0 && Unquote.UNQUOTE_SYMBOL == o) {
                    /* if UNQUOTE is just before the last element a */
                    if (n != list.size - 2) {
                        throw IllegalSyntaxException.of(UNQUOTE.toString(), list, "expects exactly one expression")
                    }
                    /* Evaluate and append last element */
                    return Append.append(result, evaluator.eval(list[n + 1], env))
                }
                if (isList(expr) && UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == o) {
                    throw IllegalSyntaxException.of(UNQUOTE_SPLICING.toString(), expr, "invalid context within quasiquote")
                }
                /* Otherwise, just append the element wrapped with LIST */
                result = Append.append(result, list(o))
            } else {
                val el = o
                val op = el[0]
                if (QUASIQUOTE_SYMBOL == op) {
                    /* Increase depth of quasiquotation */
                    result = Append.append(result, list(quasiquoteList(depth + 1, o, env, evaluator)))
                } else if (Unquote.UNQUOTE_SYMBOL == op || UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == op) {
                    if (el.size != 2) {
                        throw IllegalSyntaxException.of(op.toString(), expr, "expects exactly one expression")
                    }
                    if (depth == 0) {
                        /* Level of quasiquotation is 0 - evaluate! */
                        val eval = evaluator.eval(el[1], env)
                        if (UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == op) {
                            /* Unquote Splicing: splice and append elements into resulting list */
                            /* `(,@(list 1 2 3)) => `(1 2 3) */
                            if (eval is Collection<*>) {
                                (result as MutableList<Any?>).addAll((eval as Collection<*>?)!!)
                            } else {
                                result = eval
                            }
                        } else {
                            /* Unquote: append list with results */
                            /* `(,(list 1 2 3)) => `((1 2 3)) */
                            result = Append.append(result, list(eval))
                        }
                    } else {
                        /* Decrease depth of quasiquotation */
                        result = Append.append(result, list(quasiquoteList(depth - 1, o, env, evaluator)))
                    }
                } else {
                    result = Append.append(result, list(quasiquoteList(depth, o, env, evaluator)))
                }
            }
        }
        if (!isList) {
            /* In the case of a pair, if the cdr of the relevant quoted pair is empty,
             * then expr need not produce a list, and its result is used directly in place of the quoted pair */
            if (isNull(Cdr.cdr(result))) {
                return (result as List<*>)[0]
            } else {
                // TODO Is car(cdr(result)) correct?
                return cons(Car.car(result), Car.car(Cdr.cdr(result)))
            }
        }
        return result
    }

    // TODO Optimize vector->list and list-<vector conversions
    private fun quasiquoteVector(expr: Any, env: Environment, evaluator: Evaluator): Any {
        val vector = expr as MutableVector
        if (vector.size == 0) {
            /* Nothing to process */
            return vector
        }
        /* `#(unquote 1)  syntax is not valid */
        /* `,@#(list 1 2) syntax is not valid */
        if (Unquote.UNQUOTE_SYMBOL == vector[0] || UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == vector[0]) {
            throw IllegalSyntaxException.of(vector[0]!!.toString(), expr, "invalid context within quasiquote")
        }
        val list = VectorToList.vectorToList(expr)
        val result = quasiquoteList(0, list, env, evaluator)
        // FIXME throw "illegal use of '.'" in Reader instead
        if (!isList(result)) {
            throw IllegalSyntaxException("read: illegal use of '.'")
        }
        return ListToVector.listToVector(result)
    }

    private fun quasiquoteSet(expr: Any, env: Environment, evaluator: Evaluator): Any {
        val set = expr as Set<*>
        if (set.isEmpty()) {
            /* Nothing to process */
            return set
        }
        /* `#(unquote 1)  syntax is not valid */
        /* `,@#(list 1 2) syntax is not valid */
        val first = First.first(set)
        if (Unquote.UNQUOTE_SYMBOL == first || UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL == first) {
            throw IllegalSyntaxException.of(first.toString(), expr, "invalid context within quasiquote")
        }
        val list = Cons.list(set)
        val result = quasiquoteList(0, list, env, evaluator)
        // FIXME throw "illegal use of '.'" in Reader instead
        if (!isList(result)) {
            throw IllegalSyntaxException("read: illegal use of '.'")
        }
        return setProc(result)
    }

    override fun toString() = "quasiquote"
}
