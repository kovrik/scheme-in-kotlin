package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator

interface ISpecialForm {

    fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any?
}
