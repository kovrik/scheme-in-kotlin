package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils.toSequence

class First : AFn<Any?, Any?>(name = "first", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = first(arg)

    companion object {
        fun first(arg: Any?) = arg?.let {  toSequence(arg).firstOrNull() }
    }
}
