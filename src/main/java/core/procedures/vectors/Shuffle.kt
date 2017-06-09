package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Vector
import java.util.Collections
import kotlin.collections.ArrayList
import kotlin.collections.Collection
import kotlin.collections.toTypedArray

class Shuffle : AFn(name = "shuffle", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Vector {
        if (arg is Collection<*>) {
            val list = ArrayList((arg as Collection<*>?)!!)
            Collections.shuffle(list)
            return Vector(*list.toTypedArray())
        }
        throw WrongTypeException(name, Collection::class.java, arg)
    }
}
