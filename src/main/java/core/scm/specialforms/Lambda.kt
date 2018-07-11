package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.procedures.predicates.Predicate
import core.scm.Closure
import core.scm.Symbol
import core.Writer
import core.procedures.Arity.AtLeast
import core.procedures.Arity.Exactly
import java.util.LinkedList

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variableN> . <variableN+1>)
 */
object Lambda : SpecialForm("lambda") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Closure {
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
                /* Validate args */
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
            else -> listOf(Begin).plus(form.drop(2))
        }
        val arity = when (variadic) {
            true  -> AtLeast(0)
            false -> Exactly(params.size)
        }
        return Closure(params, body, env, arity)
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
