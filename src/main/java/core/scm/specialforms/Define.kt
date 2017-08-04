package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.scm.Symbol
import core.writer.Writer

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
object Define : ISpecialForm {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), form)
        }
        var id: Any? = form[1]
        /* Variable definition: (define <id> <value>) */
        if (id is Symbol) {
            if (form.size > 3) {
                throw IllegalSyntaxException(toString(), form, "multiple expressions after identifier")
            }
            env.put(id, evaluator.eval(form[2], env))
        } else if (id is Cons<*>) {
            /* Procedure definition: (define <id> <proc>) */
            /* Function shorthand definition
             * form = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
             *              |   0   | 1 definition           | 3 body      |
             */
            /* Args */
            val lambdaArgs = Cons.list((form[1] as List<Any>).subList(1, (form[1] as List<Any>).size) as Collection<Any>)
            lambdaArgs.forEach {
                if (it !is Symbol && !Cons.isPair(it)) {
                    throw IllegalSyntaxException(toString(), form, "not an identifier: ${Writer.write(it)}")
                }
            }
            lambdaArgs.isProperList = Cons.isProperList(form[1])
            /* Construct lambda form */
            val l = Cons.list<Any?>(Lambda).apply {
                add(lambdaArgs)
                /* Body */
                addAll(form.subList(2, form.size))
            }
            /* Get procedure's name */
            // TODO (define (((a))) 1)
            // TODO (define ((a n) c) n)
            id = id[0] as? Symbol ?: throw IllegalSyntaxException(toString(), form,
                                                                  "not an identifier for procedure name: ${Writer.write(id)}")
            val lambda = Lambda.eval(l, env, evaluator).apply { name = id.toString() }
            env.put(id, lambda)
        } else {
            throw IllegalSyntaxException(toString(), form)
        }
        return id
    }

    override fun toString() = "define"
}
