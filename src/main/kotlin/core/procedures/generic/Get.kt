package core.procedures.generic

import core.procedures.AFn
import core.scm.Vector
import core.utils.Utils

class Get : AFn<Any?, Any?>(name = "get", isPure = true, minArgs = 2, maxArgs = 3) {

    override operator fun invoke(args: Array<out Any?>) = invoke(args[0], args[1], args.getOrNull(2))

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        when (arg1) {
            is Map<*, *> -> return (arg1 as Map<Any?, Any?>).getOrDefault(arg2, arg3)
            is Map.Entry<*, *> -> if (Utils.isExactNonNegativeInteger(arg2)) {
                val i = (arg2 as Number).toInt()
                when (i) {
                    0 -> return arg1.key
                    1 -> return arg1.value
                }
            }
            is List<*> -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is Set<*> -> if (arg1.contains(arg2)) {
                return arg2
            }
            is Vector -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is CharSequence -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.length) {
                return arg1[arg2.toInt()]
            }
            is BooleanArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is CharArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is ByteArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is ShortArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is IntArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is LongArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is DoubleArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is FloatArray -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
            is Array<*> -> if (Utils.isExactNonNegativeInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
        }
        return arg3
    }
}
