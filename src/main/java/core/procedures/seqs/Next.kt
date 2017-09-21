package core.procedures.seqs

import core.procedures.AFn
import core.scm.Vector
import core.utils.Utils

class Next : AFn<Any?, Any?>(name = "next", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        null                  -> null
        !Utils.isSeqable(arg) -> throw IllegalArgumentException("don't know how to create Sequence from ${arg.javaClass}")
        is Sequence<*>        -> arg.drop(1).let { if (it.iterator().hasNext()) it else null }
        is Map.Entry<*, *>    -> Utils.toSequence(arg).let { if (!it.iterator().hasNext()) null else it.drop(1) }
        is Set<*>             -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is Map<*, *>          -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is List<*>            -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is Vector             -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is CharSequence       -> if (arg.length < 2) null else arg.asSequence().drop(1)
        is ByteArray          -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is BooleanArray       -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is CharArray          -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is ShortArray         -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is IntArray           -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is LongArray          -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is DoubleArray        -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is FloatArray         -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        is Array<*>           -> if (arg.size   < 2) null else arg.asSequence().drop(1)
        else                  -> throw IllegalArgumentException("don't know how to create Sequence from ${arg.javaClass}")
    }
}
