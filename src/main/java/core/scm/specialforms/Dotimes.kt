package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.scm.Symbol
import core.scm.Type
import core.Writer

/**
 * Syntax:
 * (dotimes (var limit [result]) <body>...)
 */
object Dotimes : SpecialForm("dotimes") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val binding = form[1] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
        val s = binding[0] as? Symbol ?: throw WrongTypeException(toString(), "Symbol", binding[0])

        val limit = evaluator.eval(binding[1])
        Type.assertType(name, limit, Type.Real::class.java)
        val localEvaluator = Evaluator(Environment(evaluator.env))
        val body = listOf(Begin) + form.drop(2)
        // TODO What if greater than Long.MAX_VALUE?
        for (i in 0 until (limit as Number).toLong()) {
            localEvaluator.env[s] = i
            localEvaluator.eval(body)
        }
        return binding.getOrNull(2)?.let { localEvaluator.eval(it) }
    }
}