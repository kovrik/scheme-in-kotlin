package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk

/* Syntax:
 * (letrec <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
/*
 * TODO One restriction on letrec is very important:
 * it must be possible to evaluate each <init> without assigning or referring to the value of any <variable>.
 * If this restriction is violated, then it is an error.
 * The restriction is necessary because Scheme passes arguments by value rather than by name.
 * In the most common uses of letrec, all the <init>s are lambda expressions and the restriction is satisfied automatically.
 */
enum class LetRec : ISpecialForm {
    LETREC;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (expression.size < 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val localEnv = Environment(env)
        val bindings = expression[1] as List<List<*>>
        /* Bind variables to fresh locations holding undefined values */
        for (binding in bindings) {
            localEnv.put(binding[0], Environment.UNDEFINED)
        }
        /* Evaluate inits */
        for (binding in bindings) {
            localEnv.put(binding[0], evaluator.eval(binding[1], localEnv))
        }
        /* Evaluate body */
        for (i in 2..expression.size - 2) {
            evaluator.eval(expression[i], localEnv)
        }
        return Thunk(expression[expression.size - 1], localEnv)
    }

    override fun toString() = "letrec"
}
