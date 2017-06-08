package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol

/* Syntax:
 * (set! <variable> <expression>)
 */
enum class Set : ISpecialForm {
    SET;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator) {
        if (expression.size != 3) {
            throw IllegalSyntaxException.of(toString(), expression, "has ${expression.size - 1} parts after keyword")
        }
        val identifier = expression[1]
        if (identifier !is Symbol) {
            throw IllegalSyntaxException.of(toString(), expression, "not an identifier: `$identifier`")
        }
        env.findAndPut(identifier, evaluator.eval(expression[2], env))
        return Unit
    }

    override fun toString() = "set!"
}
