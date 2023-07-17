package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.procedures.IFn
import core.Writer
import core.scm.Type

object DynamicWind : SpecialForm("dynamic-wind") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size != 4) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${form.size - 1} parts after keyword")
        }
        val pre = evaluator.eval(form[1])
        Type.assertType("Procedure", pre, IFn::class.java)

        val value = evaluator.eval(form[2])
        Type.assertType("Procedure", value, IFn::class.java)

        val post = evaluator.eval(form[3])
        Type.assertType("Procedure", post, IFn::class.java)

        /* Evaluate before-thunk first */
        evaluator.eval(listOf(pre))
        try {
            /* Evaluate and return value-thunk */
            return evaluator.eval(listOf(value))
        } finally {
            /* Finally, evaluate post-thunk */
            evaluator.eval(listOf(post))
        }
    }
}
