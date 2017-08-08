package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.utils.Utils

/* Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
object Do : SpecialForm("do") {

    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) {
            throw IllegalSyntaxException(toString(), form)
        }
        // TODO Replace with call to LET
        /* Init bindings */
        val bs = form[1] as? List<*> ?: throw IllegalSyntaxException(toString(), form)
        val tempEnv = Environment(env)
        val steps = Cons.list<Cons<*>>()
        for (b in bs) {
            val binding = b as? List<*> ?: throw IllegalSyntaxException(toString(), form)
            /* Check that init value exists */
            if (binding.size < 2) {
                throw IllegalSyntaxException(toString(), form)
            }
            val (variable, init) = binding
            if (binding.size == 3) {
                /* Put pair of Var and Step */
                steps.add(Cons.cons(variable, binding[2]))
            }
            /* Check that we have no duplicates among variables */
            if (tempEnv.containsKey(variable)) {
                throw IllegalSyntaxException(Let.toString(), form, "duplicate identifier: $variable")
            }
            tempEnv.put(variable, evaluator.eval(init, tempEnv))
        }

        val clause = form[2] as? List<*> ?: throw IllegalSyntaxException(toString(), form)
        if (clause.isEmpty()) {
            throw IllegalSyntaxException(toString(), form)
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
                freshLocations.put(step!!.car(), evaluator.eval(step.cdr(), tempEnv))
            }
            /* Now store results */
            tempEnv.putAll(freshLocations)
        }
        /* Test evaluated to #f */
        return Begin.eval(clause, tempEnv, evaluator)
    }
}
