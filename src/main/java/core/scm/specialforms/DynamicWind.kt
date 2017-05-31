package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.procedures.IFn
import core.scm.Cons

enum class DynamicWind : ISpecialForm {
    DYNAMIC_WIND;

    override fun toString() = "dynamic-wind"

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val size = expression.size
        if (size != 4) {
            throw IllegalSyntaxException.of(toString(), expression, "has ${size - 1} parts after keyword")
        }
        val pre = evaluator.eval(expression[1], env)
        if (pre !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", pre)
        }
        val value = evaluator.eval(expression[2], env)
        if (value !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", value)
        }
        val post = evaluator.eval(expression[3], env)
        if (post !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", post)
        }
        /* Evaluate before-thunk first */
        evaluator.eval(Cons.list<Any>(pre), env)
        try {
            /* Evaluate and return value-thunk */
            return evaluator.eval(Cons.list<Any>(value), env)
        } finally {
            /* Finally, evaluate post-thunk */
            evaluator.eval(Cons.list<Any>(post), env)
        }
    }
}
