package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.procedures.predicates.Predicate
import core.scm.Procedure
import core.scm.Symbol
import core.Writer
import java.util.LinkedList
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
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val params: List<Symbol?>
        val variadic: Boolean
        /* Check if args is a List or not */
        val args = form[1]
        when (args) {
            is List<*> -> {
                variadic = !Predicate.isProperList(args)
                params = if (variadic) {
                    /* (lambda (arg-id ...+ . rest-id) body ...+) */
                    /* args is an improper list, hence variadic lambda */
                    flatten(args as List<Symbol>)
                } else {
                    /* (lambda (arg-id ...+) body ...+) OR */
                    /* args is a proper list, hence non-variadic lambda */
                    args as List<Symbol>
                }
                /* Check args for duplicates */
                validateArgs(params, form)
            }
            is Symbol -> {
                /* Variadic arity */
                /* (lambda rest-id body ...+) */
                params = listOf(args)
                variadic = true
            }
            else -> throw IllegalSyntaxException(toString(), Writer.write(form), "bad argument sequence: ($args)")
        }
        val body = when {
            form.size == 3 -> form[2]!!
            /* Add implicit `begin` */
            else -> mutableListOf<Any?>(Begin).apply { addAll(form.subList(2, form.size)) }
        }
        return Procedure("", params.toTypedArray(), body, env, variadic)
    }

    private fun validateArgs(params: List<Any?>, form: List<Any?>) {
        if (!params.isEmpty()) {
            val temp = hashSetOf<Symbol>()
            params.forEach {
                when {
                    it !is Symbol -> throw IllegalSyntaxException(toString(), Writer.write(form), "not an identifier: $it")
                    !temp.add(it) -> throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate argument name: $it")
                }
            }
        }
    }

    /* Non-recursively flatten a list (or a chain of conses) */
    private fun <E> flatten(list: List<E>) = mutableListOf<E>().apply {
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
