package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class NumberToString : AFn(FnArgsBuilder().min(1).max(2).mandatory(arrayOf<Class<*>>(Number::class.java)).rest(Type.ExactPositiveInteger::class.java).build()) {

    override val name = "number->string"

    override operator fun invoke(vararg args: Any?): String? {
        val o = args[0] as Number
        var o1: Any? = null
        if (args.size == 2) {
            o1 = args[1]
            if (!(o1 == 2L || o1 == 8L || o1 == 10L || o1 == 16L)) {
                throw IllegalArgumentException(name + ": bad radix (must be one of: 2, 8, 10 or 16): " + o1)
            }
        }
        val radix = if (o1 != null) (o1 as Number).toInt() else 10
        if (o is Long) {
            return java.lang.Long.toString(o, radix)
        }
        if (o is Double) {
            if (radix != 10) {
                throw IllegalArgumentException(name + ": inexact numbers can only be printed in base 10")
            }
            return o.toString()
        }
        if (o is BigDecimal) {
            val bigDecimal = o
            if (radix == 10) {
                return bigDecimal.toString()
            }
            /* Check if it is integral */
            if (Utils.isInteger(bigDecimal)) {
                return bigDecimal.toBigInteger().toString(radix)
            }
            throw IllegalArgumentException(name + ": inexact numbers can only be printed in base 10")
        }
        if (o is BigInteger) {
            val bigInteger = o
            if (radix == 10) {
                return bigInteger.toString()
            }
            /* Check if it is integral */
            if (Utils.isInteger(bigInteger)) {
                return bigInteger.toString(radix)
            }
            throw IllegalArgumentException(name + ": inexact numbers can only be printed in base 10")
        }
        return o.toString()
    }
}