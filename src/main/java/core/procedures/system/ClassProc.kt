package core.procedures.system

import core.procedures.AFn

open class ClassProc : AFn<Any?, Class<*>?>(name = "class", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = arg?.javaClass
}
