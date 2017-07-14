package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Symbol
import core.scm.Thunk

/* Syntax:
 * (let <bindings> <body>)
 *
 * <bindings>: ((<variable1> <init1>) ...)
 */
object Let : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException.of(toString(), form)
        }
        /* Normal let:
         * (let ((id expr) ...) body ...+) */
        if (form[1] is List<*>) {
            val localEnv = Environment(env)
            val bindings = form[1] as List<List<*>>
            /* Bind variables to fresh locations holding undefined values */
            for (binding in bindings) {
                localEnv.put(binding[0], Environment.UNDEFINED)
            }
            /* Evaluate inits */
            for (binding in bindings) {
                val (variable, init) = binding
                if (localEnv[variable] !== Environment.UNDEFINED) {
                    throw IllegalSyntaxException.of(toString(), form, "duplicate identifier: $variable")
                }
                localEnv.put(variable, evaluator.eval(init, env))
            }

            /* Evaluate body */
            for (i in 2..form.size - 2) {
                evaluator.eval(form[i], localEnv)
            }
            return Thunk(form[form.size - 1], localEnv)

        } else if (form[1] is Symbol) {
            // TODO Optimize and cleanup
            /* Named let:
             * (let proc-id ((arg-id init-expr) ...) body ...+) */
            val name = form[1] as? Symbol ?: throw IllegalSyntaxException.of(toString(), form)
            /* Construct lambda */
            val lambdaArgs = Cons.list<Any?>()
            val initValues = Cons.list<Any?>()
            val bindings = form[2] as List<*>
            for (binding in bindings) {
                val arg = (binding as List<*>)[0]
                if (lambdaArgs.contains(arg)) {
                    throw IllegalSyntaxException.of(toString(), form, "duplicate identifier: $arg")
                }
                lambdaArgs.add(arg)
                initValues.add(binding[1])
            }
            val lambdaBody = form[3]
            val lambda = Cons.list(Lambda, lambdaArgs, lambdaBody)
            val l = Cons.list<Cons<*>>()
            l.add(Cons.list(name, lambda))

            val body = Cons.list<Any?>(name)
            body.addAll(initValues)

            /* Named let is implemented via letrec */
            val letrec = Cons.list(LetRec, l, body)
            /* Letrec has TCO */
            return LetRec.eval(letrec, Environment(env), evaluator)
        }
        throw IllegalSyntaxException.of(toString(), form)
    }

    override fun toString() = "let"
}
