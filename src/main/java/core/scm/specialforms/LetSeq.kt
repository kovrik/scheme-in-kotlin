package core.scm.specialforms

import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.Writer
import core.environment.Environment

/**
 * Syntax:
 * (let* <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object LetSeq : SpecialForm("let*") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        if (form.size < 3 || form[1] !is List<*>) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val localEvaluator = Evaluator(Environment(evaluator.env))
        val bindings = form[1] as List<*>
        /* Evaluate inits */
        bindings.forEach {
            if (it !is List<*>) throw IllegalSyntaxException(toString(), Writer.write(form))
            localEvaluator.env[it[0]] = localEvaluator.eval(it[1])
        }
        /* Evaluate body */
        (2..form.size - 2).forEach { localEvaluator.eval(form[it]) }
        return Thunk(form[form.size - 1], localEvaluator.env)
    }
}
