package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Type
import core.scm.Vector
import core.utils.Utils

class Into : AFn<Any?, Any?>(name = "into", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        return when (arg1) {
            is Sequence<*> -> arg1.plus(Utils.toSequence(arg2))
            is List<*> -> Cons.list<Any?>().apply {
                addAll(arg1)
                addAll(Utils.toSequence(arg2))
            }
            is Set<*> -> HashSet<Any?>().apply {
                addAll(arg1)
                addAll(Utils.toSequence(arg2))
            }
            is Vector -> {
                val seq = Utils.toSequence(arg2)
                MutableVector(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) { this[i] = seq.elementAt(i) }
                }
            }
            is ByteArray -> {
                val seq = Utils.toSequence(arg2)
                ByteArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Byte::class.java)
                        this[i] = (e as Number).toByte()
                    }
                }
            }
            is BooleanArray -> {
                val seq = Utils.toSequence(arg2)
                BooleanArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Boolean::class.java)
                        this[i] = e as Boolean
                    }
                }
            }
            is CharArray -> {
                val seq = Utils.toSequence(arg2)
                CharArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Char::class.java)
                        this[i] = (e as Number).toChar()
                    }
                }
            }
            is ShortArray -> {
                val seq = Utils.toSequence(arg2)
                ShortArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Short::class.java)
                        this[i] = (e as Number).toShort()
                    }
                }
            }
            is IntArray -> {
                val seq = Utils.toSequence(arg2)
                IntArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Int::class.java)
                        this[i] = (e as Number).toInt()
                    }
                }
            }
            is LongArray -> {
                val seq = Utils.toSequence(arg2)
                LongArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Long::class.java)
                        this[i] = (e as Number).toLong()
                    }
                }
            }
            is DoubleArray -> {
                val seq = Utils.toSequence(arg2)
                DoubleArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Double::class.java)
                        this[i] = (e as Number).toDouble()
                    }
                }
            }
            is FloatArray -> {
                val seq = Utils.toSequence(arg2)
                FloatArray(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        Type.assertType(name, e, Float::class.java)
                        this[i] = (e as Number).toFloat()
                    }
                }
            }
            is Array<*> -> {
                val seq = Utils.toSequence(arg2)
                arrayOfNulls<Any?>(arg1.size + seq.count()).apply {
                    for (i in 0 until arg1.size)    { this[i] = arg1[i] }
                    for (i in arg1.size until size) {
                        val e = seq.elementAt(i)
                        this[i] = e
                    }
                }
            }
            // TODO Map?
            else -> throw WrongTypeException(name, "Seqable", arg1)
        }
    }
}