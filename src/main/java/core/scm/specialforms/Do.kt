package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils
import core.Writer

/**
 * Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
object Do : SpecialForm("do") {

    override fun eval(form: List<Any?>, evaluator: Evaluator): Any {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        // TODO Replace with call to LET
        /* Init bindings */
        val bs = form[1] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
        val tempEvaluator = Evaluator(Environment(evaluator.env))
        val steps = hashMapOf<Any?, Any?>()
        for (b in bs) {
            val binding = b as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
            /* Check that init value exists */
            if (binding.size < 2) {
                throw IllegalSyntaxException(toString(), Writer.write(form))
            }
            val (variable, init) = binding
            if (binding.size == 3) {
                /* Put pair of Var and Step */
                steps[variable] = binding[2]
            }
            /* Check that we have no duplicates among variables */
            if (tempEvaluator.env.containsKey(variable)) {
                throw IllegalSyntaxException(Let.toString(), Writer.write(form), "duplicate identifier: $variable")
            }
            tempEvaluator.env[variable] = tempEvaluator.eval(init)
        }

        val clause = form[2] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
        clause.ifEmpty { throw IllegalSyntaxException(toString(), Writer.write(form)) }
        /* While test evaluates to #f */
        while (!Utils.toBoolean(tempEvaluator.eval(clause[0]))) {
            /* Evaluate command expressions */
            form.drop(3).forEach { f ->
                /* Each iteration establishes bindings to fresh locations
                 * See https://www.gnu.org/software/guile/manual/html_node/while-do.html */
                val fresh = Environment(evaluator.env)
                fresh.putAll(tempEvaluator.env)
                /* Evaluate using new fresh environment */
                Evaluator(fresh).eval(f)
                /* Then put results into tempEnv */
                tempEvaluator.env.putAll(fresh)
            }
            /* Evaluate steps and now store results */
            tempEvaluator.env.putAll(steps.entries.associate { it.key to tempEvaluator.eval(it.value) })
        }
        /* Test evaluated to #f */
        return Begin.eval(clause, tempEvaluator)
    }
}
