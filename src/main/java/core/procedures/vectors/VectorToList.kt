package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class VectorToList : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Vector::class.java)).build()) {

    companion object {
        fun vectorToList(v: MutableVector): Cons<*> {
            return Cons.list<Any>(*v.getArray())
        }
    }

    override val name: String
        get() = "vector->list"

    override fun apply1(arg: Any?): Cons<*>? {
        return vectorToList(arg as MutableVector)
    }
}
