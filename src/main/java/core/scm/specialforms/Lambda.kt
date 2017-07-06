package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Procedure
import core.scm.Symbol
import java.util.LinkedList

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
enum class Lambda : ISpecialForm {
    LAMBDA;

    override fun toString() = "lambda"

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Procedure {
        if (form.size < 3) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val params: List<Symbol?>
        var variadic = false
        /* Check if args is a List or not */
        val lambdaArgs = form[1]
        if (lambdaArgs is List<*>) {
            /* Check args for duplicates */
            if (!lambdaArgs.isEmpty()) {
                val temp = HashSet<Any?>(lambdaArgs.size)
                for (o in lambdaArgs) {
                    if (o !is Symbol && !Cons.isPair(o)) {
                        throw IllegalSyntaxException.of(toString(), form, "not an identifier: $o")
                    }
                    if (temp.contains(o)) {
                        throw IllegalSyntaxException.of(toString(), form, "duplicate argument name: $o")
                    }
                    temp.add(o)
                }
            }
            /* (lambda (arg-id ...+) body ...+) OR
             * (lambda (arg-id ...+ . rest-id) body ...+) */
            if (Cons.isProperList(lambdaArgs)) {
                /* args is a proper list, hence non-variadic lambda */
                params = lambdaArgs as List<Symbol>
            } else {
                /* args is an improper list, hence variadic lambda */
                params = flatten(lambdaArgs as List<Symbol>)
                variadic = true
            }
        } else {
            /* Variadic arity */
            /* (lambda rest-id body ...+) */
            if (lambdaArgs !is Symbol) {
                throw IllegalSyntaxException("lambda: bad argument sequence ($lambdaArgs) in form: $form")
            }
            params = Cons.list(lambdaArgs)
            variadic = true
        }
        val body: Any?
        if (form.size == 3) {
            body = form[2]
        } else {
            /* Add implicit `begin` */
            body = Cons.list(Begin.BEGIN)
            (body as MutableList<Any?>).addAll(form.subList(2, form.size))
        }
        return Procedure("", params.toTypedArray(), body, env, variadic)
    }

    /* Non-recursively flatten a list (or a chain of conses) */
    fun <E> flatten(list: List<E>): List<E> {
        val result = ArrayList<E>()
        val queue = LinkedList(list)
        while (!queue.isEmpty()) {
            val e = queue.remove()
            if (e is List<*>) {
                queue.addAll(e as List<E>)
            } else {
                result.add(e)
            }
        }
        return result
    }
}
