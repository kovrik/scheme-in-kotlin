package core.procedures.collections

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.*

class Empty : AFn<Any?, Any?>(name = "empty", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?): Any? = when (arg) {
        is Sequence<*>   -> emptySequence<Nothing>()
        is List<*>       -> mutableListOf<Nothing>()
        is Set<*>        -> MutableSet<Nothing>()
        is MutableVector -> MutableVector.EMPTY
        is Vector        -> Vector.EMPTY
        is Map<*, *>     -> Hashmap<Nothing, Nothing>()
        is CharSequence  -> MutableString(0)
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
