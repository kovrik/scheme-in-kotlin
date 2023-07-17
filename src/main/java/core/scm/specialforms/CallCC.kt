package core.scm.specialforms

import core.Evaluator
import core.Writer
import core.exceptions.IllegalSyntaxException
import core.exceptions.WrongTypeException
import core.procedures.IFn
import core.scm.CalledContinuation
import core.scm.Continuation

object CallCC : SpecialForm("call-with-current-continuation") {

    /* Actual call-with-current-continuation */
    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size != 2) {
            throw IllegalSyntaxException(toString(), Writer.write(this))
        }
        val proc = evaluator.eval(form[1])
        if (proc !is IFn<*, *>) {
            throw WrongTypeException(toString(), "Procedure", proc)
        }
        val cont = Continuation()
        return try {
            /* Pass Continuation to the Procedure: (proc cont) */
            evaluator.eval(listOf(proc, cont))
        } catch (ex: CalledContinuation) {
            if (ex.continuation != cont) {
                /* Not our continuation, throw it further */
                throw ex
            }
            /* Our continuation, grab and return the resulting value */
            ex.value
        } finally {
            /* One-shot continuations cannot be used more than once */
            cont.invoked = true
        }
    }
}
