package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Vector

import java.util.ArrayList
import java.util.Collections

class Shuffle : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "shuffle"

    override operator fun invoke(arg: Any?): Vector {
        if (arg is Collection<*>) {
            val list = ArrayList((arg as Collection<*>?)!!)
            Collections.shuffle(list)
            return Vector(*list.toTypedArray())
        }
        throw WrongTypeException(name, Collection::class.java, arg)
    }
}
