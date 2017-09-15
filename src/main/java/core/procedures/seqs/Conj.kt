package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

class Conj : AFn<Any?, Any?>(name = "conj", minArgs = 1) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1) return args[0]
        val first = args[0]
        return when (first) {
            is Sequence<*> -> first.plus(args.copyOfRange(1, args.size))
            is List<*> -> Cons.list<Any?>().apply {
                addAll(first)
                addAll(args.copyOfRange(1, args.size))
            }
            is Set<*> -> HashSet<Any?>().apply {
                addAll(first as Collection<*>)
                addAll(args.copyOfRange(1, args.size))
            }
            is Vector -> MutableVector(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this.array, first.size, args.size - 1)
            }
            is ByteArray -> ByteArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is BooleanArray -> BooleanArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is CharArray -> CharArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is ShortArray -> ShortArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is IntArray -> IntArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is LongArray -> LongArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is DoubleArray -> DoubleArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is FloatArray -> FloatArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            is Array<*> -> arrayOfNulls<Any?>(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                System.arraycopy(args, 1, this, first.size, args.size - 1)
            }
            // TODO Map?
            else -> throw WrongTypeException(name, "Seqable", first)
        }
    }
}
