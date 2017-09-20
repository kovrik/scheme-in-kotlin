package core.procedures.collections

import core.procedures.AFn
import core.scm.*

class Empty : AFn<Any?, Any?>(name = "empty", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        is Sequence<*>   -> emptySequence<Any?>()
        is List<*>       -> Cons.list<Any>()
        is Set<*>        -> MutableHashSet<Any?>()
        is Vector        -> Vector.EMPTY
        is MutableVector -> MutableVector.EMPTY
        is Map<*, *>     -> Hashmap()
        is BooleanArray  -> BooleanArray(0)
        is CharArray     -> CharArray(0)
        is ByteArray     -> ByteArray(0)
        is ShortArray    -> ShortArray(0)
        is IntArray      -> IntArray(0)
        is LongArray     -> LongArray(0)
        is DoubleArray   -> DoubleArray(0)
        is FloatArray    -> FloatArray(0)
        is Array<*>      -> arrayOf<Any?>()
        else             -> null
    }
}
