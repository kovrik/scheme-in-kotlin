package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk

/* Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object LetSeq : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val localEnv = Environment(env)
        val bindings = form[1] as List<List<*>>
        /* Evaluate inits */
        bindings.forEach { localEnv.put(it[0], evaluator.eval(it[1], localEnv)) }
        /* Evaluate body */
        for (i in 2..form.size - 2) {
            evaluator.eval(form[i], localEnv)
        }
        return Thunk(form[form.size - 1], localEnv)
    }

    override fun toString() = "let*"
}
