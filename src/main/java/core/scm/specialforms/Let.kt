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
enum class Let : ISpecialForm {
    LET;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any {
        if (expression.size < 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        /* Normal let:
         * (let ((id expr) ...) body ...+) */
        if (expression[1] is List<*>) {
            val localEnv = Environment(env)
            val bindings = expression[1] as List<List<*>>
            /* Bind variables to fresh locations holding undefined values */
            for (binding in bindings) {
                localEnv.put(binding[0], Environment.UNDEFINED)
            }
            /* Evaluate inits */
            for (binding in bindings) {
                val variable = binding[0]
                val init = binding[1]
                if (localEnv[variable] !== Environment.UNDEFINED) {
                    throw IllegalSyntaxException
                            .of(toString(), expression, String.format("duplicate identifier: %s", variable))
                }
                localEnv.put(variable, evaluator.eval(init, env))
            }

            /* Evaluate body */
            for (i in 2..expression.size - 1 - 1) {
                evaluator.eval(expression[i], localEnv)
            }
            return Thunk(expression[expression.size - 1], localEnv)

        } else if (expression[1] is Symbol) {
            // TODO Optimize and cleanup
            /* Named let:
             * (let proc-id ((arg-id init-expr) ...) body ...+) */
            val name = expression[1] as? Symbol ?: throw IllegalSyntaxException.of(toString(), expression)
            /* Construct lambda */
            val lambdaArgs = Cons.list<Any?>()
            val initValues = Cons.list<Any?>()
            val bindings = expression[2] as List<*>
            for (binding in bindings) {
                val arg = (binding as List<*>)[0]
                if (lambdaArgs.contains(arg)) {
                    throw IllegalSyntaxException
                            .of(toString(), expression, String.format("duplicate identifier: %s", arg))
                }
                lambdaArgs.add(arg)
                initValues.add(binding[1])
            }
            val lambdaBody = expression[3]
            val lambda = Cons.list(Lambda.LAMBDA, lambdaArgs, lambdaBody)
            val l = Cons.list<Cons<*>>()
            l.add(Cons.list(name, lambda))

            val body = Cons.list<Any?>(name)
            body.addAll(initValues)

            /* Named let is implemented via letrec */
            val letrec = Cons.list<Any>(LetRec.LETREC, l, body)
            /* Letrec has TCO */
            return LetRec.LETREC.eval(letrec, Environment(env), evaluator)
        }
        throw IllegalSyntaxException.of(toString(), expression)
    }

    override fun toString(): String {
        return "let"
    }
}
