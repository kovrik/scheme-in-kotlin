package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.*

class Conj : AFn<Any?, Any?>(name = "conj", arity = AtLeast(1)) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1) return args.first()
        return when (val first = args.first()) {
            is Sequence<*> -> first + args.copyOfRange(1, args.size)
            is List<*>     -> (first + args.copyOfRange(1, args.size)).toList()
            is Set<*>      -> MutableSet(first + args.copyOfRange(1, args.size))
            is Vector      -> MutableVector(first + args.copyOfRange(1, args.size))
            is BooleanArray -> BooleanArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = args[i] as? Boolean ?: throw WrongTypeException(name, Boolean::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is ByteArray -> ByteArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toByte() ?: throw WrongTypeException(name, Int::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is CharArray -> CharArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toChar() ?: throw WrongTypeException(name, Char::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is ShortArray -> ShortArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toShort() ?: throw WrongTypeException(name, Int::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is IntArray -> IntArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toInt() ?: throw WrongTypeException(name, Int::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is LongArray -> LongArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toLong() ?: throw WrongTypeException(name, Long::class.javaObjectType, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is DoubleArray -> DoubleArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toDouble() ?: throw WrongTypeException(name, Type.Real::class.java, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is FloatArray -> FloatArray(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size) {
                    val e = (args[i] as? Number)?.toFloat() ?: throw WrongTypeException(name, Type.Real::class.java, args[i])
                    this[first.size + i - 1] = e
                }
            }
            is Array<*> -> arrayOfNulls<Any?>(first.size + args.size - 1).apply {
                for (i in 0 until first.size) { this[i] = first[i] }
                for (i in 1 until args.size)  { this[first.size + i - 1] = args[i] }
            }
            is Pair<*, *>  -> throw UnsupportedOperationException("$name: Pairs are not supported!")
            is MutablePair<*, *> -> throw UnsupportedOperationException("$name: Mutable Pairs are not supported!")
            // TODO Map?
            else -> throw WrongTypeException(name, "Seqable", first)
        }
    }
}
