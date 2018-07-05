package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.utils.Utils
import java.util.*

class Sort : AFn<Any?, Any?>(name = "sort", isPure = true, arity = Range(1, 2)) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size == 1 || args[0] == null -> (args[0] ?: args[1]).let {
            (Utils.toSequence(it) as Sequence<Comparable<Any?>>).sortedWith(naturalOrder())
        }
        else -> (Utils.toSequence(args[1]) as Sequence<Comparable<Any?>>).sortedWith(args[0] as Comparator<Any?>)
    }
}
