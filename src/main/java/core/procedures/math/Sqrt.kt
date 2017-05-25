package core.procedures.math

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.BigComplex

class Sqrt : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Number::class.java)).build()) {

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

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "sqrt"

    override operator fun invoke(arg: Any?): Number? {
        return sqrt(arg as Number?)
    }
}
