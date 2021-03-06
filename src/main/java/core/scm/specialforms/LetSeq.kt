package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.Writer

/**
 * Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object LetSeq : SpecialForm("let*") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3 || form[1] !is List<*>) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val localEnv = Environment(env)
        val bindings = form[1] as List<*>
        /* Evaluate inits */
        bindings.forEach {
            if (it !is List<*>) throw IllegalSyntaxException(toString(), Writer.write(form))
            localEnv[it[0]] = evaluator.eval(it[1], localEnv)
        }
        /* Evaluate body */
        (2..form.size - 2).forEach { evaluator.eval(form[it], localEnv) }
        return Thunk(form[form.size - 1], localEnv)
    }
}
