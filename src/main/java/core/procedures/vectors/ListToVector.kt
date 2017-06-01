package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Type

class ListToVector : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Type.ProperList::class.java))) {

    companion object {
        fun listToVector(arg: Any?) = MutableVector(*(arg as List<*>).toTypedArray())
    }

    override val name = "list->vector"
    override operator fun invoke(arg: Any?) = listToVector(arg)
}
