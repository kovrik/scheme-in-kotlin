package core.procedures.bit

import core.procedures.AFn
import core.scm.Type
import core.utils.Utils
import java.math.BigInteger

class ArithmeticShift : AFn<Number?, Number>(name = "arithmetic-shift", isPure = true, minArgs = 2, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf(Type.BitOpOrBigInt::class.java, Long::class.javaObjectType)) {

    override operator fun invoke(arg1: Number?, arg2: Number?): Number = when {
        Utils.isPositive(arg2!!) -> when (arg1) {
            is BigInteger -> arg1.shiftLeft(arg2.toInt())
            else -> arg1!!.toLong() shl arg2.toInt()
        }
        else -> when (arg1) {
            is BigInteger -> arg1.shiftRight(-arg2.toInt())
            else -> arg1!!.toLong() shr -arg2.toInt()
        }
    }
}
