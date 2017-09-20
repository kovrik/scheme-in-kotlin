package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn<Any?, Unit>(name = "void", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = Unit
    override operator fun invoke() = Unit
    override operator fun invoke(arg: Any?) = Unit
    override operator fun invoke(arg1: Any?, arg2: Any?) = Unit
    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = Unit
    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?, arg4: Any?) = Unit
    override fun checkArity(args: Array<out Any?>) = Unit
    override fun checkArgs(args: Array<out Any?>) = Unit
}
