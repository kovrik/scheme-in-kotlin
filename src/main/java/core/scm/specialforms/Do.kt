package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.exceptions.IllegalSyntaxException
import core.utils.Utils
import core.Writer

/* Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
object Do : SpecialForm("do") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        // TODO Replace with call to LET
        /* Init bindings */
        val bs = form[1] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
        val tempEnv = Environment(env)
        val steps = mutableListOf<Pair<*, *>>()
        for (b in bs) {
            val binding = b as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
            /* Check that init value exists */
            if (binding.size < 2) {
                throw IllegalSyntaxException(toString(), Writer.write(form))
            }
            val (variable, init) = binding
            if (binding.size == 3) {
                /* Put pair of Var and Step */
                steps.add(Pair(variable, binding[2]))
            }
            /* Check that we have no duplicates among variables */
            if (tempEnv.containsKey(variable)) {
                throw IllegalSyntaxException(Let.toString(), Writer.write(form), "duplicate identifier: $variable")
            }
            tempEnv[variable] = evaluator.eval(init, tempEnv)
        }

        val clause = form[2] as? List<*> ?: throw IllegalSyntaxException(toString(), Writer.write(form))
        if (clause.isEmpty()) {
            throw IllegalSyntaxException(toString(), Writer.write(form))
        }
        /* While test evaluates to #f */
        while (!Utils.toBoolean(evaluator.eval(clause[0], tempEnv))) {
            /* Evaluate command expressions */
            for (e in form.subList(3, form.size)) {
                /* Each iteration establishes bindings to fresh locations
                 * See https://www.gnu.org/software/guile/manual/html_node/while-do.html */
                val environment = Environment(env).apply { putAll(tempEnv) }
                /* Evaluate using new fresh environment */
                evaluator.eval(e, environment)
                /* THen put results into tempEnv */
                tempEnv.putAll(environment)
            }
            /* Evaluate steps */
            val freshLocations = HashMap<Any?, Any?>(steps.size)
            for (step in steps) {
                freshLocations[step.first] = evaluator.eval(step.second, tempEnv)
            }
            /* Now store results */
            tempEnv.putAll(freshLocations)
        }
        /* Test evaluated to #f */
        return Begin.eval(clause, tempEnv, evaluator)
    }
}
