package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.reader.Reader
import core.utils.Utils

class StringToNumber : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(CharSequence::class.java), rest = Long::class.java)) {

    override val name = "string->number"

    override operator fun invoke(vararg args: Any?): Any? {
        val number = args[0].toString()
        /* Check if we should override optional radix */
        /* Read radix and/or exactness and a number */
        var override = false
        var radixChar: Char? = null
        var exactness: Char? = null
        var restNumber = number
        while (restNumber.length > 1 && restNumber[0] == '#') {
            val ch = restNumber[1]
            if (Reader.isExactness(ch)) {
                if (exactness != null) {
                    return false
                }
                exactness = ch
                restNumber = restNumber.substring(2)
                continue
            }
            if (Reader.isRadix(ch)) {
                if (radixChar != null) {
                    return false
                }
                radixChar = ch
                restNumber = restNumber.substring(2)
                override = true
                continue
            }
            break
        }
        if (restNumber.isEmpty()) {
            return false
        }
        var radix = Utils.getRadixByChar(radixChar)
        /* Get default (optional) radix if present */
        if (args.size == 2) {
            val optRadix = (args[1] as Number).toInt()
            if (optRadix < 2 || optRadix > 16) {
                throw IllegalArgumentException(name + ": expected radix from 2 to 16")
            }
            if (!override) {
                radix = optRadix
            }
        }

        /* Read number */
        val result = Utils.preProcessNumber(restNumber, exactness, radix)
        if (result is Number) {
            return result
        }
        return false
    }
}