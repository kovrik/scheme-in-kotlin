package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.scm.Syntax
import core.Writer

/* Syntax:
 * (syntax <template>)
 */
object SyntaxForm : SpecialForm("syntax") {

    // TODO Nested forms?
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator) = when (form.size) {
        2    -> Syntax(form[1])
        else -> throw IllegalSyntaxException(toString(), Writer.write(form))
    }
}