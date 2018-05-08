package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.scm.Symbol
import core.scm.Type
import core.Writer

/* Syntax:
 * (dotimes (var limit [result]) <body>...)
 */
object Dotimes : SpecialForm("dotimes") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val binding = form[1] as List<*>
        val s = binding[0] as? Symbol ?: throw WrongTypeException(toString(), "Symbol", binding[0])

        val limit = evaluator.eval(binding[1], env)
        Type.assertType(name, limit, Type.Real::class.java)
        val localEnv = Environment(env)
        val body = mutableListOf<Any?>(Begin).apply { addAll(form.subList(2, form.size)) }
        // TODO What if greater than Long.MAX_VALUE?
        for (i in 0 until (limit as Number).toLong()) {
            localEnv.put(s, i)
            evaluator.eval(body, localEnv)
        }
        return binding.getOrNull(2)?.let { evaluator.eval(it, env) }
    }
}