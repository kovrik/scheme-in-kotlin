package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.procedures.IFn
import core.Writer

object DynamicWind : SpecialForm("dynamic-wind") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = form.size
        if (size != 4) {
            throw IllegalSyntaxException(toString(), Writer.write(form), "has ${size - 1} parts after keyword")
        }
        val pre = evaluator.eval(form[1], env)
        if (pre !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", pre)
        }
        val value = evaluator.eval(form[2], env)
        if (value !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", value)
        }
        val post = evaluator.eval(form[3], env)
        if (post !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", post)
        }
        /* Evaluate before-thunk first */
        evaluator.eval(listOf<Any>(pre), env)
        try {
            /* Evaluate and return value-thunk */
            return evaluator.eval(listOf<Any>(value), env)
        } finally {
            /* Finally, evaluate post-thunk */
            evaluator.eval(listOf<Any>(post), env)
        }
    }
}
