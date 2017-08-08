package core.scm.specialforms

import core.environment.Environment
import core.evaluator.Evaluator
import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.procedures.IFn
import core.procedures.functional.Apply
import core.scm.Box
import core.scm.Cons

object Swap : SpecialForm("swap!") {

    private val apply = Apply()

    // TODO Check and optimize. Try to implement as a Procedure
    override fun eval(form: List<Any?>, env: Environment, evaluator: Evaluator): Any? {
        if (form.size < 3) throw ArityException(name, 2, Integer.MAX_VALUE, form.size)
        val b = evaluator.eval(form[1], env)
        val box = b as? Box<Any?> ?: throw WrongTypeException(name, "Box", b)

        val f = evaluator.eval(form[2], env)
        val fn  = f as? IFn<Any?, Any?> ?: throw WrongTypeException(name, "Procedure", f)
        val rest = Cons.list<Any?>()
        if (form.size > 3) {
            for (i in 3..form.size - 1) {
                rest.add(evaluator.eval(form[i], env))
            }
        }
        val applyForm  = Cons.list(apply, fn, Cons.list(Quote, rest))
        while (true) {
            val oldVal = box.deref()
            rest.add(0, oldVal)
            val newVal = evaluator.eval(applyForm, env)
            if (box.compareAndSet(oldVal, newVal)) {
                return newVal
            }
        }
    }
}
