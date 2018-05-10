package core.procedures.box

import core.procedures.AFn
import core.procedures.IFn
import core.procedures.functional.Apply
import core.scm.Box
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Let
import core.scm.specialforms.Quote

class Swap : AFn<Any?, Any?>(name = "swap!", minArgs = 2, mandatoryArgsTypes = arrayOf(Box::class.java, IFn::class.java)) {

    private val apply = Apply()
    private val setBox = SetBox()

    // TODO Check!
    override fun invoke(args: Array<out Any?>): Thunk<Any?> {
        val box = args[0] as Box<Any?>
        val fn = args[1] as? IFn<*, *>
        while (true) {
            val oldVal = box.deref()
            val rest = mutableListOf(oldVal)
            if (args.size > 2) {
                for (i in 2 until args.size) {
                    rest.add(args[i])
                }
            }
            val newVal = Symbol.intern("newVal")
            val thunk = Thunk(listOf(Let, listOf(listOf(newVal, listOf(apply, fn, Quote.quote(rest)))), listOf(setBox, box, newVal), newVal))
            if (box.compareAndSet(oldVal, thunk)) {
                return thunk
            }
        }
    }
}
