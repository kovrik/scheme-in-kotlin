package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.Type

class DegreesToRadians : AFn<Number?, Number>(name = "degrees->radians", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    companion object {
        const val rad = Math.PI / 180
    }

    override operator fun invoke(arg: Number?) = arg!!.toDouble() * rad
}