package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn<Any?, Unit>(name = "void", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = Unit
}
