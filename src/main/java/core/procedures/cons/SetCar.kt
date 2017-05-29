package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type
import core.scm.Void

class SetCar : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Type.Pair::class.java, Any::class.java))) {

    override val name = "set-car!"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        (arg1 as MutableList<Any?>)[0] = arg2
        return Void
    }
}
