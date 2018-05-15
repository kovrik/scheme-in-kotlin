package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Symbol
import core.Writer

/* Syntax:
 * (define <variable> <expression>)
 * (define (<variable> <formals>) <body>)
 * (define (<variable> . <formal>) <body>)
 */
object Define : SpecialForm("define") {

    private val cdr = Cdr()

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        var id: Any? = form[1]
        /* Variable definition: (define <id> <value>) */
        when (id) {
            is Symbol -> {
                if (form.size > 3) {
                    throw IllegalSyntaxException(toString(), Writer.write(form), "multiple expressions after identifier")
                }
                env[id] = evaluator.eval(form[2], env)
            }
            is List<*> -> {
                /* Procedure definition: (define <id> <proc>) */
                /* Function shorthand definition
                 * form = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
                 *              |   0   | 1 definition           | 3 body      |
                 */
                id.subList(1, id.size).forEach {
                    if (it !is Symbol && !Predicate.isPair(it)) {
                        throw IllegalSyntaxException(toString(), Writer.write(form), "not an identifier: ${Writer.write(it)}")
                    }
                }
                /* Construct lambda form: (lambda (args) body) */
                val lambda = mutableListOf(Lambda, cdr(id)).apply { addAll(form.subList(2, form.size)) }
                /* Get procedure's name */
                // TODO (define (((a))) 1)
                // TODO (define ((a n) c) n)
                id = id[0] as? Symbol ?: throw IllegalSyntaxException(toString(), Writer.write(form),
                                               "not an identifier for procedure name: ${Writer.write(id)}")
                env[id] = Lambda.eval(lambda, env, evaluator)
            }
            else -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        return id
    }
}
