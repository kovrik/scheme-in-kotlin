package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutablePair
import core.scm.Type
import core.scm.Vector
import core.utils.Utils

class Next : AFn<Any?, Any?>(
    name = "next", isPure = true, arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        null -> null
        is Sequence<*> -> arg.drop(1).let { if (it.any()) it else null }
        is Pair<*, *> -> sequenceOf(arg.second)
        is MutablePair<*, *> -> sequenceOf(arg.second)
        is Map.Entry<*, *> -> Utils.toSequence(arg).let { if (it.any()) it.drop(1) else null }
        is Set<*> -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is Map<*, *> -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is List<*> -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is Vector -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is CharSequence -> if (arg.length < 2) null else arg.asSequence().drop(1)
        is ByteArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is BooleanArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is CharArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is ShortArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is IntArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is LongArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is DoubleArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is FloatArray -> if (arg.size < 2) null else arg.asSequence().drop(1)
        is Array<*> -> if (arg.size < 2) null else arg.asSequence().drop(1)
        else -> throw IllegalArgumentException("don't know how to create Sequence from ${arg.javaClass}")
    }
}
