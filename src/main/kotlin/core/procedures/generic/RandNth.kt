package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils
import java.util.*

class RandNth : AFn<Any?, Any?>(name = "rand-nth", isPure = true, minArgs = 1, maxArgs = 1) {

    private val count = Count()
    private val get = Get()

    override operator fun invoke(arg: Any?) = when {
        arg is Map<*, *>      -> throw UnsupportedOperationException("nth not supported on this type: ${arg.javaClass}")
        !Utils.isSeqable(arg) -> throw IllegalArgumentException("don't know how to create Sequence from ${arg?.javaClass}")
        else -> count(arg).let {
            when (it) {
                0 -> throw IndexOutOfBoundsException()
                else -> get(arg, Random().nextInt(it), null)
            }
        }
    }
}
