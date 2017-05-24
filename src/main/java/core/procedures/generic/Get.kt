package core.procedures.generic

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Vector
import core.utils.Utils

class Get : AFn(FnArgsBuilder().min(2).max(3).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "get"

    override fun apply(vararg args: Any?): Any? {
        var defaultValue: Any? = null
        if (args.size == 3) {
            defaultValue = args[2]
        }
        return apply3(args[0], args[1], defaultValue)
    }

    override fun apply3(arg1: Any?, arg2: Any?, arg3: Any?): Any? {
        if (arg1 is Map<*, *>) {
            return (arg1 as Map<Any?, Any?>).getOrDefault(arg2, arg3)
        } else if (arg1 is Map.Entry<*, *>) {
            if (Utils.isInteger(arg2)) {
                val i = (arg2 as Number).toInt()
                when (i) {
                    0 -> return arg1.key
                    1 -> return arg1.value
                }
            }
        } else if (arg1 is List<*>) {
            if (Utils.isInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
        } else if (arg1 is Set<*>) {
            if (arg1.contains(arg2)) {
                return arg2
            }
        } else if (arg1 is Vector) {
            if (Utils.isInteger(arg2) && (arg2 as Number).toInt() < arg1.size) {
                return arg1[arg2.toInt()]
            }
        } else if (arg1 is CharSequence) {
            if (Utils.isInteger(arg2) && (arg2 as Number).toInt() < arg1.length) {
                return arg1[arg2.toInt()]
            }
        }
        return arg3
    }
}
