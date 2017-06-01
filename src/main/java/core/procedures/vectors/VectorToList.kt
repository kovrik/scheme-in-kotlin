package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class VectorToList : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Vector::class.java))) {

    companion object {
        fun vectorToList(v: MutableVector) = Cons.list(*v.getArray())
    }

    override val name = "vector->list"
    override operator fun invoke(arg: Any?) = vectorToList(arg as MutableVector)
}
