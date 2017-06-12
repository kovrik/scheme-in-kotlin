package core.procedures.generic

import core.procedures.AFn
import core.utils.Utils
import java.util.*

class RandNth : AFn<Any?, Any?>(name = "rand-nth", isPure = true, minArgs = 1, maxArgs = 1) {

    private val count = Count()
    private val get = Get()

    override operator fun invoke(arg: Any?): Any? {
        if (arg is Map<*, *>) {
            throw UnsupportedOperationException("nth not supported on this type: ${arg.javaClass}")
        }
        if (!Utils.isSeqable(arg)) {
            throw IllegalArgumentException("don't know how to create Sequence from ${arg?.javaClass}")
        }
        val bound = count(arg)
        if (bound == 0) {
            throw IndexOutOfBoundsException()
        }
        val index = Random().nextInt(bound)
        return get(arg, index, null)
    }
}
