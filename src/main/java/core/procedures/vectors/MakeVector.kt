package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Type

class MakeVector : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java))) {

    override val name = "make-vector"

    override operator fun invoke(vararg args: Any?): Any {
        val s = (args[0] as Number).toLong()
        var init: Any? = null
        if (args.size == 2) {
            init = args[1]
        }
        return MutableVector(s.toInt(), init)
    }
}
