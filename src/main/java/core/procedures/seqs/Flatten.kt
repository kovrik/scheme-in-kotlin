package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.scm.cached
import core.utils.Utils

class Flatten : AFn<Any?, Any?>(
    name = "flatten", arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Sequence<Any?> = FlatteningIterator(Utils.toSequence(arg)).asSequence().cached()

    private class FlatteningIterator(private var queue: Sequence<Any?>) : Iterator<Any?> {

        override fun hasNext() = queue.any()

        override tailrec fun next(): Any? {
            val obj = queue.first()
            queue = queue.drop(1)
            return when (Utils.isSeqable(obj)) {
                true -> {
                    queue = Utils.toSequence(obj) + queue
                    next()
                }
                false -> obj
            }
        }
    }
}