package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn(name = "void", isPure = true) {

    override operator fun invoke(vararg args: Any?) = Unit

    companion object {
        internal val VOID = VoidProc()
    }
}
