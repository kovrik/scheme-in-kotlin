package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.Type
import kotlin.math.PI

class DegreesToRadians : AFn<Number?, Number>(name = "degrees->radians", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    companion object {
        const val RAD = PI / 180
    }

    override operator fun invoke(arg: Number?) = arg!!.toDouble() * RAD
}