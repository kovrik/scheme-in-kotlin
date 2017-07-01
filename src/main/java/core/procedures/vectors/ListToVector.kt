package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Type

class ListToVector : AFn<List<*>?, MutableVector>(name = "list->vector", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Type.ProperList::class.java)) {

    override operator fun invoke(arg: List<*>?) = listToVector(arg)

    companion object {
        fun listToVector(arg: List<*>?) = MutableVector(arg!!.toTypedArray())
    }
}
