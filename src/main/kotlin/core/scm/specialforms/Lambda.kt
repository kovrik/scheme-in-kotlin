package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Procedure
import core.scm.Symbol
import java.util.LinkedList
import kotlin.collections.ArrayList
import kotlin.collections.HashSet

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
object Lambda : SpecialForm("lambda") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Procedure {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), form)
        }
        val params: List<Symbol?>
        var variadic = false
        /* Check if args is a List or not */
        val lambdaArgs = form[1]
        if (lambdaArgs is List<*>) {
            /* Check args for duplicates */
            if (!lambdaArgs.isEmpty()) {
                val temp = HashSet<Any?>(lambdaArgs.size)
                lambdaArgs.forEach {
                    when {
                        it !is Symbol && !Cons.isPair(it) -> throw IllegalSyntaxException(toString(), form, "not an identifier: $it")
                        temp.contains(it) -> throw IllegalSyntaxException(toString(), form, "duplicate argument name: $it")
                        else -> temp.add(it)
                    }
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
            params = Cons.list(lambdaArgs as? Symbol ?:
                               throw IllegalSyntaxException("lambda: bad argument sequence ($lambdaArgs) in form: $form"))
            variadic = true
        }
        val body = when {
            form.size == 3 -> form[2]!!
            /* Add implicit `begin` */
            else -> Cons.list<Any?>(Begin).apply { addAll(form.subList(2, form.size)) }
        }
        return Procedure("", params.toTypedArray(), body, env, variadic)
    }

    /* Non-recursively flatten a list (or a chain of conses) */
    private fun <E> flatten(list: List<E>) = ArrayList<E>().apply {
        val queue = LinkedList(list)
        while (!queue.isEmpty()) {
            val e = queue.remove()
            when (e) {
                is List<*> -> queue.addAll(e as List<E>)
                else -> add(e)
            }
        }
    }
}
