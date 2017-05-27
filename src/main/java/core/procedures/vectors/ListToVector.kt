package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.scm.MutableVector

class ListToVector : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Type.ProperList::class.java)).build()) {

    companion object {
        fun listToVector(arg: Any?): MutableVector {
            return MutableVector(*(arg as List<*>).toTypedArray())
        }
    }

    override val name = "list->vector"

    override operator fun invoke(arg: Any?): MutableVector {
        return listToVector(arg)
    }
}
