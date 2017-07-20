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
object LetRec : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        val localEnv = Environment(env)
        val bindings = form[1] as List<List<*>>
        /* Bind variables to fresh locations holding undefined values */
        bindings.forEach { localEnv.put(it[0], Environment.UNDEFINED) }
        /* Evaluate inits */
        bindings.forEach { localEnv.put(it[0], evaluator.eval(it[1], localEnv)) }
        /* Evaluate body */
        for (i in 2..form.size - 2) {
            evaluator.eval(form[i], localEnv)
        }
        return Thunk(form[form.size - 1], localEnv)
    }

    override fun toString() = "letrec"
}
