package core.procedures.vectors

import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class VectorToList : AFn(name = "vector->list", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Vector::class.java)) {

    override operator fun invoke(arg: Any?) = vectorToList(arg as MutableVector)

    companion object {
        fun vectorToList(v: MutableVector) = Cons.list(*v.getArray())
    }
}
