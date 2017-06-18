package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils

class Second : AFn<Any?, Any?>(name = "second", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = second(arg)

    companion object {
        fun second(arg: Any?) = arg?.let { Utils.toSequence(it).elementAtOrNull(1) }
    }
}
