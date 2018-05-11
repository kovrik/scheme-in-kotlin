package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Thunk
import core.Writer

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
object LetRec : SpecialForm("letrec") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3 || form[1] !is List<*>) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        val localEnv = Environment(env)
        val bindings = form[1] as List<*>
        /* Bind variables to fresh locations holding undefined values */
        bindings.forEach {
            if (it !is List<*>) throw IllegalSyntaxException(toString(), Writer.write(form))
            localEnv[it[0]] = Environment.UNDEFINED
        }
        /* Evaluate inits */
        bindings.forEach {
            with (it as List<*>) { localEnv.put(it[0], evaluator.eval(it[1], localEnv)) }
        }
        /* Evaluate body */
        for (i in 2..form.size - 2) {
            evaluator.eval(form[i], localEnv)
        }
        return Thunk(form[form.size - 1], localEnv)
    }
}
