package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn() {

    companion object {
        internal val VOID = VoidProc()
    }

    override val isPure = true
    override val name = "void"
    override operator fun invoke(vararg args: Any?) = Unit
}
