package core.procedures.strings

import core.procedures.AFn
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class NumberToString : AFn<Any?, String>(name = "number->string", isPure = true, minArgs = 1, maxArgs = 2,
                           mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java), restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<out Any?>): String {
        val number = args[0]!! as Number
        val o1 = args.getOrNull(1)
        o1?.let {
            if (!(o1 == 2L || o1 == 8L || o1 == 10L || o1 == 16L)) {
                throw IllegalArgumentException("$name: bad radix (must be one of: 2, 8, 10 or 16): $o1")
            }
        }
        val radix = if (o1 != null) (o1 as Number).toInt() else 10
        if (number is Long) {
            return java.lang.Long.toString(number, radix)
        }
        if (number is Double) {
            if (radix != 10) {
                throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
            return number.toString()
        }
        if (number is BigDecimal) {
            val bigDecimal = number
            return when {
                radix == 10 -> bigDecimal.toString()
                /* Check if it is integral */
                Utils.isInteger(bigDecimal) -> bigDecimal.toBigInteger().toString(radix)
                else -> throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
        }
        if (number is BigInteger) {
            val bigInteger = number
            return when {
                radix == 10 -> bigInteger.toString()
                /* Check if it is integral */
                Utils.isInteger(bigInteger) -> bigInteger.toString(radix)
                else -> throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
        }
        return number.toString()
    }
}