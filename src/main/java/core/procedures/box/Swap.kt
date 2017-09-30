package core.procedures.box

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Box

class Swap : AFn<Any?, Any?>(name = "swap!", minArgs = 2,
                             mandatoryArgsTypes = arrayOf<Class<*>>(Box::class.java, IFn::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?) {
        TODO("TBD. Use Scheme version for now")
    }
}