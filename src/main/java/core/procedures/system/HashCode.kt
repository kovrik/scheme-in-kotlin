package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

open class HashCode : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "hashcode"

    override operator fun invoke(arg: Any?): Any? {
        return arg?.hashCode() ?: 0L
    }
}
