package core.procedures.arrays

import core.procedures.AFn

class ToArray : AFn<Collection<*>?, Any?>(name = "to-array", isPure = true, minArgs = 1, maxArgs = 1, lastArgType = Collection::class.java) {

    override operator fun invoke(arg: Collection<*>?) = arg?.toTypedArray()
}
