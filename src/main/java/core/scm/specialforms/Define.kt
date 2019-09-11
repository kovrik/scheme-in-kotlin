package core.scm.specialforms

import core.Evaluator
import core.environment.Environment
import core.exceptions.IllegalSyntaxException
import core.procedures.cons.Cdr
import core.procedures.predicates.Predicate
import core.scm.Symbol
import core.Writer
import core.utils.Utils

/**
 * Syntax:
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
            is List<*>, is Pair<*, *> -> {
                /* Procedure definition: (define <id> <proc>) */
                /* Function shorthand definition
                 * form = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
                 *              |   0   | 1 definition           | 3 body      |
                 */
                val seq = Utils.toSequence(id)
                seq.forEach {
                    if (it !is Symbol && !Predicate.isPairOrNonEmptyList(it)) {
                        throw IllegalSyntaxException(toString(), Writer.write(form), "not an identifier: ${Writer.write(it)}")
                    }
                }
                /* Construct lambda form: (lambda (args) body) */
                val lambda = listOf(Lambda, cdr(id)) + form.drop(2)
                /* Get procedure's name */
                // TODO (define (((a))) 1)
                // TODO (define ((a n) c) n)
                id = seq.first() as? Symbol ?: throw IllegalSyntaxException(toString(), Writer.write(form),
                        "not an identifier for procedure name: ${Writer.write(id)}")
                env[id] = Lambda.eval(lambda, env, evaluator)
            }
            else -> throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        return id
    }
}
