package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex

class Sqrt : AFn(name = "sqrt", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Any?) = sqrt(arg as Number?)

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
}
