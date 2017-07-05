package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator

/**
 * Comment Special Form:
 * Ignores body and returns nothing
 * Syntax:
 * (comment <expression1> ... <expression n>)
 */
enum class Comment : ISpecialForm {
    COMMENT;

    override fun toString() = "comment"
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = Unit
}
