package core.procedures.math

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Ratio
import core.scm.Type

import java.math.BigDecimal
import java.math.RoundingMode
import kotlin.math.truncate

class Truncate : AFn<Number?, Number>(name = "truncate", isPure = true, arity = Exactly(1),
                                      mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?): Number = when (arg) {
        is Double -> truncate(arg)
        is Float  -> truncate(arg)
        is BigDecimal -> {
            val result = when {
                arg.signum() < 0 -> arg.setScale(0, RoundingMode.UP)
                else -> arg.setScale(0, RoundingMode.DOWN)
            }
            when {
                arg.scale() > 0 -> result.setScale(1)
                else -> result
            }
        }
        is Ratio -> arg.truncate()
        else -> arg!!
    }
}
