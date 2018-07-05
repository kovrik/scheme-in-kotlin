package core.procedures.strings

import core.procedures.AFn
import core.procedures.Arity.Range
import core.utils.Utils

import java.math.BigDecimal
import java.math.BigInteger

class NumberToString : AFn<Any?, String>(name = "number->string", isPure = true, arity = Range(1, 2),
                                         mandatoryArgsTypes = arrayOf(Number::class.java), restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<out Any?>): String {
        val number = args[0]!! as Number
        val o1 = (args.getOrNull(1) as Long?)
        o1?.let {
            if (o1 < Character.MIN_RADIX || o1 > Character.MAX_RADIX) {
                throw IllegalArgumentException("$name: bad radix (must be from 2 to 36 inclusive): $o1")
            }
        }
        val radix = if (o1 != null) (o1 as Number).toInt() else 10
        if (Utils.isInexact(number)) {
            if (radix != 10) {
                throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
            return number.toString()
        }
        if (number is BigDecimal) {
            return when {
                radix == 10 -> number.toString()
                /* Check if it is integral */
                Utils.isInteger(number) -> number.toBigInteger().toString(radix)
                else -> throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
        }
        if (number is BigInteger) {
            return when {
                radix == 10 -> number.toString()
                /* Check if it is integral */
                Utils.isInteger(number) -> number.toString(radix)
                else -> throw IllegalArgumentException("$name: inexact numbers can only be printed in base 10")
            }
        }
        return when (number) {
            is Long, is Int, is Short, is Byte -> java.lang.Long.toString(number.toLong(), radix)
            else -> number.toString()
        }
    }
}