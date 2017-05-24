package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Symbol
import core.scm.Void

/* Syntax:
 * (set! <variable> <expression>)
 */
enum class Set : ISpecialForm {
    SET;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Void? {
        if (expression.size != 3) {
            throw IllegalSyntaxException
                    .of(toString(), expression, String.format("has %s parts after keyword", expression.size - 1))
        }
        val identifier = expression[1]
        if (identifier !is Symbol) {
            throw IllegalSyntaxException
                    .of(toString(), expression, String.format("not an identifier: `%s`", identifier))
        }
        env.findAndPut(identifier, evaluator.eval(expression[2], env))
        return Void.VOID
    }

    override fun toString(): String {
        return "set!"
    }
}
