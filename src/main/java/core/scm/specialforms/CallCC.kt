package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.WrongTypeException
import core.procedures.IFn
import core.scm.CalledContinuation
import core.scm.Continuation
import core.scm.Cons

object CallCC : ISpecialForm {

    override fun toString() = "call-with-current-continuation"

    /* Actual call-with-current-continuation */
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        val proc = evaluator.eval(form[1], env)
        if (proc !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", proc)
        }
        val cont = Continuation()
        try {
            /* Pass Continuation to the Procedure: (proc cont) */
            return evaluator.eval(Cons.list(proc, cont), env)
        } catch (ex: CalledContinuation) {
            if (ex.continuation != cont) {
                /* Not our continuation, throw it further */
                throw ex
            }
            /* Our continuation, grab and return the resulting value */
            return ex.value
        } finally {
            /* One-shot continuations cannot be used more than once */
            cont.invalidate()
        }
    }
}
