package core.scm.specialforms

import core.environment.Environment
import core.Evaluator
import core.procedures.IFn
import core.scm.CalledContinuation
import core.scm.Continuation
import core.scm.Type

object CallCC : SpecialForm("call-with-current-continuation") {

    /* Actual call-with-current-continuation */
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val proc = evaluator.eval(form[1], env)
        Type.assertType(name, proc, IFn::class.java)
        val cont = Continuation()
        return try {
            /* Pass Continuation to the Procedure: (proc cont) */
            evaluator.eval(listOf(proc, cont), env)
        } catch (ex: CalledContinuation) {
            if (ex.continuation != cont) {
                /* Not our continuation, throw it further */
                throw ex
            }
            /* Our continuation, grab and return the resulting value */
            ex.value
        } finally {
            /* One-shot continuations cannot be used more than once */
            cont.isInvoked = true
        }
    }
}
