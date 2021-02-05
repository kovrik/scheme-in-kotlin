package core.procedures.strings

import core.exceptions.IllegalSyntaxException
import core.procedures.AFn
import core.procedures.Arity.Range
import core.reader.Reader
import core.utils.Utils

class StringToNumber : AFn<Any?, Any?>(name = "string->number", isPure = true, arity = Range(1, 2),
                                       mandatoryArgsTypes = arrayOf(CharSequence::class.java),
                                       restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<out Any?>): Any {
        val number = args[0].toString()
        /* Check if we should override optional radix */
        /* Read radix and/or exactness and a number */
        var override = false
        var radixChar: Char? = null
        var exact: Boolean? = null
        var restNumber = number
        while (restNumber.length > 1 && restNumber[0] == '#') {
            val ch = restNumber[1]
            if (Reader.isExactness(ch)) {
                exact?.let { return false }
                exact = Reader.isExact(ch)
            }
            if (Reader.isRadix(ch)) {
                radixChar?.let { return false }
                radixChar = ch
                override = true
            }
            restNumber = restNumber.drop(2)
            continue
        }
        if (restNumber.isEmpty()) {
            return false
        }
        var radix = Utils.getRadixByChar(radixChar)
        /* Get default (optional) radix if present */
        if (args.size == 2) {
            val optRadix = (args[1] as Number).toInt()
            if (optRadix < Character.MIN_RADIX || optRadix > Character.MAX_RADIX) {
                throw IllegalArgumentException("$name: expected radix from 2 to 36")
            }
            if (!override) {
                radix = optRadix
            }
        }
        /* Read number */
        return try {
            Utils.preProcessNumber(restNumber, exact, radix) as? Number ?: false
        } catch (e: IllegalSyntaxException) {
            false
        }
    }
}