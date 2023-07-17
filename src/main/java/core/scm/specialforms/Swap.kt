package core.scm.specialforms

import core.Evaluator
import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.procedures.Arity.AtLeast
import core.procedures.IFn
import core.procedures.functional.Apply
import core.scm.Box

object Swap : SpecialForm("swap!") {

    private val apply = Apply()

    // TODO Check and optimize. Try to implement as a Procedure
    override fun eval(form: List<Any?>, evaluator: Evaluator): Any? {
        if (form.size < 3) throw ArityException(name, AtLeast(2), form.size)
        val box = evaluator.eval(form[1]).let { it as? Box<Any?> ?: throw WrongTypeException(name, "Box", it) }
        val fn = evaluator.eval(form[2]).let { it as? IFn<Any?, Any?> ?: throw WrongTypeException(name, "Procedure", it) }
        while (true) {
            val oldVal = box.deref()
            val rest = when (form.size > 3) {
                true  -> listOf(oldVal) + form.drop(3).map { evaluator.eval(it) }
                false -> listOf(oldVal)
            }
            val applyForm = listOf(apply, fn, Quote.quote(rest))
            val newVal = evaluator.eval(applyForm)
            if (box.compareAndSet(oldVal, newVal)) {
                return newVal
            }
        }
    }
}
