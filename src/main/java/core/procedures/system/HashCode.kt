package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

open class HashCode : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "hashcode"

    override operator fun invoke(arg: Any?): Any? {
        return arg!!.hashCode()
    }
}
