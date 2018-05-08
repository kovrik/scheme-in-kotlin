package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.MutableVector

class Vec : AFn<Any?, MutableVector>(name = "vec", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Collection<*> -> MutableVector(arg.toTypedArray())
        is CharSequence -> MutableVector(arg.length, null).apply {
            for (i in 0 until arg.length) {
                this[i] = arg[i]
            }
        }
        else -> throw WrongTypeException(name, "List or Vector or String", arg)
    }
}
