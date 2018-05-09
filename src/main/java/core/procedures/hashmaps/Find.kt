package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.IAssoc
import core.scm.MapEntry
import core.utils.Utils

class Find : AFn<Any?, MapEntry<*, *>?>(name = "find", isPure = true, maxArgs = 2, minArgs = 2,
                                        mandatoryArgsTypes = arrayOf(IAssoc::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Utils.toAssoc<Any?, Any?>(arg1).getEntry(arg2)
}
