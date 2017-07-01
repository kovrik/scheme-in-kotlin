package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn<Any?, Unit>(name = "void", isPure = true) {

    override operator fun invoke(args: Array<Any?>) = Unit

    companion object {
        internal val VOID = VoidProc()
    }
}
