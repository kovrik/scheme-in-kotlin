package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

open class HashCode : AFn<Any?, Int>(name = "hashcode", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = arg?.hashCode() ?: 0
}
