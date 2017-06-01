package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Procedure
import core.scm.Symbol

/* Syntax:
 * (lambda <formals> <body>)
 *
 * <formals>:
 * (<variable1> ...)
 * <variable>
 * (<variable1> ... <variablen> . <variablen+1>)
 */
enum class Lambda : ISpecialForm {
    LAMBDA;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Procedure {
        if (expression.size < 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }

        val params: List<Symbol?>
        var variadic = false
        /* Check if args is a List or not */
        val args = expression[1]
        if (args is List<*>) {
            /* Check args for duplicates */
            if (!args.isEmpty()) {
                val temp = HashSet<Any?>(args.size)
                for (o in args) {
                    if (o !is Symbol && !Cons.isPair(o)) {
                        throw IllegalSyntaxException.of(toString(), expression, "not an identifier: $o")
                    }
                    if (temp.contains(o)) {
                        throw IllegalSyntaxException.of(toString(), expression, "duplicate argument name: $o")
                    }
                    temp.add(o)
                }
            }
            /* (lambda (arg-id ...+) body ...+) OR
             * (lambda (arg-id ...+ . rest-id) body ...+) */
            if (Cons.isProperList(args)) {
                /* args is a proper list, hence non-variadic lambda */
                params = args as List<Symbol>
            } else {
                /* args is an improper list, hence variadic lambda */
                params = Cons.flatten(args as List<Symbol>)
                variadic = true
            }
        } else {
            /* Variadic arity */
            /* (lambda rest-id body ...+) */
            if (args !is Symbol) {
                throw IllegalSyntaxException("lambda: bad argument sequence ($args) in form: $expression")
            }
            params = Cons.list(args)
            variadic = true
        }
        val body: Any?
        if (expression.size == 3) {
            body = expression[2]
        } else {
            /* Add implicit `begin` */
            body = Cons.list(Begin.BEGIN)
            (body as MutableList<Any?>).addAll(expression.subList(2, expression.size))
        }
        return Procedure("", params.toTypedArray(), body, env, variadic)
    }

    override fun toString() = "lambda"
}
