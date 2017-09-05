package core.procedures.system

import core.procedures.AFn

open class HashCode : AFn<Any?, Int>(name = "hashcode", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = arg?.hashCode() ?: 0
}
