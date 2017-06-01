package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.BigComplex

class Sqrt : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Number::class.java))) {

    companion object {
        fun sqrt(number: Number?): Number {
            if (number is BigComplex) {
                if (number.im.signum() == 0) {
                    return sqrt(number.re)
                }
                return number.sqrt()
            }
            return Math.sqrt(number!!.toDouble())
        }
    }

    override val isPure = true
    override val name = "sqrt"
    override operator fun invoke(arg: Any?) = sqrt(arg as Number?)
}
