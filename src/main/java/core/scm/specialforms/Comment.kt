package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.scm.Void

/**
 * Comment Special Form:
 * Ignores body and returns nothing
 * Syntax:
 * (comment <expression1> ... <expression n>)
 */
enum class Comment : ISpecialForm {
    COMMENT;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Void? {
        return Void
    }

    override fun toString(): String {
        return "comment"
    }
}
