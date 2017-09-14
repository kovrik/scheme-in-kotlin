package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Get : AFn<Any?, Any?>(name = "get", isPure = true, minArgs = 2, maxArgs = 3) {

    override operator fun invoke(args: Array<out Any?>) = invoke(args[0], args[1], args.getOrNull(2))

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        when {
            arg1 is Map<*, *> -> return (arg1 as Map<Any?, Any?>).getOrDefault(arg2, arg3)
            Utils.isSeqable(arg1) -> if (Utils.isExactNonNegativeInteger(arg2)) {
                return Utils.toSequence(arg1).elementAtOrNull((arg2 as Number).toInt()) ?: arg3
            }
        }
        return arg2
    }
}
