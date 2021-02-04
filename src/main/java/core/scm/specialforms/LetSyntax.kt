package core.scm.specialforms

import core.Evaluator
import core.environment.Environment

object LetSyntax : SpecialForm("let-syntax") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = TODO("Not implemented yet")
}
