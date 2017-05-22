package core.procedures.functional

import core.procedures.AFn

class VoidProc : AFn() {

    companion object {
        internal val VOID = VoidProc()
    }

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "void"

    override fun apply(args: Array<Any?>): Any? {
        return core.scm.Void.VOID
    }
}
