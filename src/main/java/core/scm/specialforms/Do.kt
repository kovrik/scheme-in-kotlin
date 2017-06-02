package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.IllegalSyntaxException
import core.scm.Cons
import core.utils.Utils
import java.util.*

/* Syntax:
 * (do <bindings> <clause> <body>)
 *
 * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
 * <clause>:   (<test> <expression> ...),
 **/
enum class Do : ISpecialForm {
    DO;

    override fun eval(expression: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (expression.size < 3) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        // TODO Replace with call to LET
        /* Init bindings */
        val bs = expression[1] as? List<*> ?: throw IllegalSyntaxException.of(toString(), expression)
        val tempEnv = Environment(env)
        val steps = Cons.list<Cons<*>>()
        for (b in bs) {
            if (b !is List<*>) {
                throw IllegalSyntaxException.of(toString(), expression)
            }
            val binding = b
            /* Check that init value exists */
            if (binding.size < 2) {
                throw IllegalSyntaxException.of(toString(), expression)
            }
            val (variable, init) = binding
            if (binding.size == 3) {
                /* Put pair of Var and Step */
                val step = binding[2]
                steps.add(Cons.cons(variable, step))
            }
            /* Check that we have no duplicates among variables */
            if (tempEnv.containsKey(variable)) {
                throw IllegalSyntaxException.of(Let.LET.toString(), expression, "duplicate identifier: $variable")
            }
            tempEnv.put(variable, evaluator.eval(init, tempEnv))
        }

        val clause = expression[2] as? List<*> ?: throw IllegalSyntaxException.of(toString(), expression)
        if (clause.isEmpty()) {
            throw IllegalSyntaxException.of(toString(), expression)
        }
        val test = clause[0]
        val body = expression.subList(3, expression.size)
        /* While test evaluates to #f */
        while (!Utils.toBoolean(evaluator.eval(test, tempEnv))) {
            /* Evaluate command expressions */
            for (e in body) {
                /* Each iteration establishes bindings to fresh locations
                 * See https://www.gnu.org/software/guile/manual/html_node/while-do.html */
                val environment = Environment(env)
                environment.putAll(tempEnv)
                /* Evaluate using new fresh environment */
                evaluator.eval(e, environment)
                /* THen put results into tempEnv */
                tempEnv.putAll(environment)
            }
            /* Evaluate steps */
            val freshLocations = HashMap<Any?, Any?>(steps.size)
            for (step in steps) {
                val variable = step!!.car()
                val s = step.cdr()
                freshLocations.put(variable, evaluator.eval(s, tempEnv))
            }
            /* Now store results */
            for ((key, value) in freshLocations) {
                tempEnv.put(key, value)
            }
        }
        /* Test evaluated to #f */
        return Begin.BEGIN.eval(clause, tempEnv, evaluator)
    }

    override fun toString() = "do"
}
