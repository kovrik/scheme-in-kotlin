package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableVector
import core.scm.Type

class ListToVector : AFn<List<*>?, MutableVector>(name = "list->vector", isPure = true, arity = Exactly(1),
                                                  mandatoryArgsTypes = arrayOf(Type.ProperList::class.java)) {

    override operator fun invoke(arg: List<*>?) = MutableVector(arg!!.toTypedArray())
}
