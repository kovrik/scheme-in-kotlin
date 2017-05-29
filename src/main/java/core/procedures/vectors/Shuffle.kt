package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Vector

import java.util.ArrayList
import java.util.Collections

class Shuffle : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "shuffle"

    override operator fun invoke(arg: Any?): Vector {
        if (arg is Collection<*>) {
            val list = ArrayList((arg as Collection<*>?)!!)
            Collections.shuffle(list)
            return Vector(*list.toTypedArray())
        }
        throw WrongTypeException(name, Collection::class.java, arg)
    }
}
