package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Get : AFn<Any?, Any?>(name = "get", isPure = true, minArgs = 2, maxArgs = 3) {

    override operator fun invoke(args: Array<out Any?>) = invoke(args[0], args[1], args.getOrNull(2))

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = when {
        arg1 is Map<*, *> -> arg1.getOrDefault(arg2, arg3)
        Utils.isSeqable(arg1) -> when {
            Utils.isExactNonNegativeInteger(arg2) -> Utils.toSequence(arg1).elementAtOrNull((arg2 as Number).toInt()) ?: arg3
            else -> arg2
        }
        else -> arg2
    }
}
