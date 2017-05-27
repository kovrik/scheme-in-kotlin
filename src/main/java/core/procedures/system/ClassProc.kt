package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

open class ClassProc : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure = true
    override val name = "class"

    override operator fun invoke(arg: Any?): Class<*>? {
        return arg?.javaClass
    }
}
