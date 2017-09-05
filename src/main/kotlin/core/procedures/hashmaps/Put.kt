package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.IAssoc
import core.utils.Utils

class Put : AFn<Any?, Any?>(name = "put", isPure = true, minArgs = 3,
                mandatoryArgsTypes = arrayOf<Class<*>>(IAssoc::class.java, Any::class.java, Any::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = Utils.toAssoc(args[0]).let {
        for (i in 1..args.size - 2 step 2) { it.assoc(args[i], args[i + 1]) }
        it.assoc(args[args.size - 2], args[args.size - 1])
    }
}