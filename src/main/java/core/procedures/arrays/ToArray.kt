package core.procedures.arrays

import core.procedures.AFn
import core.procedures.Arity.Exactly

class ToArray : AFn<Collection<*>?, Any?>(name = "to-array", isPure = true, arity = Exactly(1), lastArgType = Collection::class.java) {

    override operator fun invoke(arg: Collection<*>?) = arg?.toTypedArray()
}
