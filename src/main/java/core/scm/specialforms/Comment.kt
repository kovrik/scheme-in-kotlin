package core.scm.specialforms

import core.Evaluator

/**
 * Comment Special Form:
 * Ignores body and returns nothing
 * Syntax:
 * (comment <expression1> ... <expression n>)
 */
object Comment : SpecialForm("comment") {

    override fun eval(form: List<Any?>, evaluator: Evaluator) = Unit
}
