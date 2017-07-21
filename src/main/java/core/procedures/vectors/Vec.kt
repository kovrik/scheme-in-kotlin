package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.MutableVector

class Vec : AFn<Any?, MutableVector>(name = "vec", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): MutableVector {
        when (arg) {
            is Collection<*> -> return MutableVector(arg.toTypedArray())
            is CharSequence -> {
                val size = arg.length
                val vector = MutableVector(size, null)
                for (i in 0..size - 1) { vector[i] = arg[i] }
                return vector
            }
            else -> throw WrongTypeException(name, "List or Vector or String", arg)
        }
    }
}
