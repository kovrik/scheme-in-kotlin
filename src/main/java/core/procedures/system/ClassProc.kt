package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

open class ClassProc : AFn<Any?, Class<*>?>(name = "class", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = arg?.javaClass
}
