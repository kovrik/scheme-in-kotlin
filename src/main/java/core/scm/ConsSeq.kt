package core.scm

import core.utils.Utils

// TODO Replace Cons with this class and rename it to Cons!
class ConsSeq(first: Any?, rest: Any?) : Sequence<Any?> {

    private val seq = when {
        Utils.isSeqable(rest) -> sequenceOf(first) + Utils.toSequence(rest)
        else -> sequenceOf(first, rest)
    }

    override fun iterator() = seq.iterator()
}