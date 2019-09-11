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
import core.utils.Utils
import java.util.LinkedList

/**
 * Syntax:
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
        /* Check if args is a List or not */
        val args = form[1]
        val variadic = args is Symbol || !Predicate.isProperList(args)
        val params = when (args) {
            is List<*>, is Pair<*, *> -> when (variadic) {
                true  -> validateParamsList(flatten(Utils.toSequence(args)).asSequence(), form)
                false -> validateParamsList(Utils.toSequence(args), form)
            }
            /* Variadic arity */
            /* (lambda rest-id body ...+) */
            is Symbol -> listOf(args)
            else -> throw IllegalSyntaxException(toString(), Writer.write(form), "bad argument sequence: ($args)")
        }
        /* Get the body and add implicit Begin if multiple body forms */
        val body  = if (form.size == 3) form[2] else listOf(Begin) + form.drop(2)
        val arity = if (variadic) AtLeast(0) else Exactly(params.size)
        return Closure(params, body, env, arity)
    }

    /* Validate args */
    private fun validateParamsList(seq: Sequence<*>, form: List<Any?>): List<Symbol?> {
        if (seq.any()) {
            val temp = hashSetOf<Symbol>()
            seq.forEach {
                when {
                    it !is Symbol -> throw IllegalSyntaxException(toString(), Writer.write(form), "not an identifier: ${Writer.write(it)}")
                    !temp.add(it) -> throw IllegalSyntaxException(toString(), Writer.write(form), "duplicate argument name: $it")
                }
            }
        }
        @Suppress("UNCHECKED_CAST")
        return (seq as Sequence<Symbol?>).toList()
    }

    /* Non-recursively flatten a list (or a chain of conses) */
    private fun flatten(seq: Sequence<*>) = mutableListOf<Any?>().apply {
        val queue = LinkedList(seq.toList())
        while (!queue.isEmpty()) {
            when (val e = queue.remove()) {
                is Pair<*, *> -> queue.apply {
                    add(e.first)
                    add(e.second)
                }
                is List<*> -> queue.addAll(e)
                else -> add(e)
            }
        }
    }
}
