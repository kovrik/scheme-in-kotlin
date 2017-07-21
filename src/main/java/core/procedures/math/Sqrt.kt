package core.procedures.math

import core.procedures.AFn
import core.scm.BigComplex

class Sqrt : AFn<Number?, Number>(name = "sqrt", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Number::class.java)) {

    override operator fun invoke(arg: Number?) = sqrt(arg!!)

    companion object {
        fun sqrt(number: Number?): Number = when (number) {
            is BigComplex -> {
                when {
                    number.im.signum() == 0 -> sqrt(number.re)
                    else -> number.sqrt()
                }
            }
            else -> Math.sqrt(number!!.toDouble())
        }
    }
}
