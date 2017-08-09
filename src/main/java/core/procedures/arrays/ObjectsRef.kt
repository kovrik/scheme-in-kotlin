package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class ObjectsRef : AFn<Any?, Any?>(name = "objects-ref", isPure = true, minArgs = 2, maxArgs = 2,
                                   mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val objects = arg1 as Array<*>
        val pos = (arg2 as Number).toInt()
        if (pos >= objects.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return objects[pos]
    }
}
