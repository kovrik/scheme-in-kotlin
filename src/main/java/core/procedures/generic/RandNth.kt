package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.utils.Utils
import java.util.*

class RandNth : AFn(FnArgsBuilder().min(1).max(1).build()) {

    private val count = Count()
    private val get = Get()

    override val isPure = true
    override val name = "rand-nth"

    override operator fun invoke(arg: Any?): Any? {
        if (arg is Map<*, *>) {
            throw UnsupportedOperationException("nth not supported on this type: " + arg.javaClass)
        }
        if (!Utils.isSeqable(arg)) {
            throw IllegalArgumentException("don't know how to create Sequence from " + arg?.javaClass)
        }
        val bound = count(arg)!!
        if (bound == 0) {
            throw IndexOutOfBoundsException()
        }
        val index = Random().nextInt(bound)
        return get(arg, index, null)
    }
}
