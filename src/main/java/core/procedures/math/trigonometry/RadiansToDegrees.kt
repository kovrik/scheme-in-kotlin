package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.Type
import kotlin.math.PI

class RadiansToDegrees : AFn<Number?, Number>(name = "radians->degrees", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    companion object {
        const val DEGREE = 180 / PI
    }

    override operator fun invoke(arg: Number?) = arg!!.toDouble() * DEGREE
}