package core.procedures.vectors

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableVector
import core.scm.Vector
import java.util.Collections
import kotlin.collections.Collection

class Shuffle : AFn<Collection<*>?, Vector>(name = "shuffle", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Collection<*>?) = when (arg) {
        is Collection<*> -> MutableVector(listOf(arg).apply(Collections::shuffle))
        else             -> throw WrongTypeException(name, Collection::class.java, arg)
    }
}
