package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.utils.Utils
import java.util.*

class Flatten : AFn<Any?, Any?>(name = "flatten", arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = flatten(Utils.toSequence(arg))

    // TODO Check, optimize and simplify
    private fun flatten(obj: Any?): List<Any?> = mutableListOf<Any?>().apply {
        when (Utils.isSeqable(obj)) {
            true -> {
                val queue = LinkedList<Any?>().apply { addAll(Utils.toSequence(obj)) }
                while (!queue.isEmpty()) {
                    val e = queue.pop()
                    when (Utils.isSeqable(e)) {
                        true  -> queue.addAll(flatten(e))
                        false -> add(e)
                    }
                }
            }
            false -> add(obj)
        }
    }
}