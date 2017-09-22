package core.procedures.seqs

import core.procedures.AFn
import core.scm.MutablePair
import core.utils.Utils
import java.util.*

class Flatten : AFn<Any?, Any?>(name = "flatten", minArgs = 1) {

    override operator fun invoke(args: Array<out Any?>) = flatten(args.map { Utils.toSequence(it) })

    // TODO Return Sequence
    private fun flatten(list: List<Any?>): List<Any?> {
        val result = mutableListOf<Any?>()
        val queue = LinkedList<Any?>()
        queue.addAll(list)
        while (!queue.isEmpty()) {
            val e = queue.remove()
            when (e) {
                // FIXME: see testFlatten()
                is Sequence<*> -> queue.addAll(e)
                is MutablePair -> {
                    queue.add(e.first)
                    queue.add(e.second)
                }
                is Iterable<*> -> queue.addAll(e)
                else           -> result.add(e)
            }
        }
        return result
    }
}