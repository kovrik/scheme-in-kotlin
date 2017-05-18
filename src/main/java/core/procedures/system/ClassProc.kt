package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

open class ClassProc : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "class"

    override fun apply1(arg: Any): Class<*> {
        return arg.javaClass
    }
}
