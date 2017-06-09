package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Type

class ListToVector : AFn(name = "list->vector", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Type.ProperList::class.java)) {

    override operator fun invoke(arg: Any?) = listToVector(arg)

    companion object {
        fun listToVector(arg: Any?) = MutableVector(*(arg as List<*>).toTypedArray())
    }
}
