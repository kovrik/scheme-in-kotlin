package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.scm.MutableVector

class MakeVector : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java)).build()) {

    override val name: String
        get() = "make-vector"

    override fun apply(vararg args: Any?): Any {
        val s = (args[0] as Number).toLong()
        var init: Any? = null
        if (args.size == 2) {
            init = args[1]
        }
        return MutableVector(s.toInt(), init)
    }
}
