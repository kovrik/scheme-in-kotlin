package core.procedures.math.trigonometry

import core.procedures.AFn
import core.scm.Type

class RadiansToDegrees : AFn<Number?, Number>(name = "radians->degrees", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(Type.Real::class.java)) {

    companion object {
        const val degree = 180 / Math.PI
    }

    override operator fun invoke(arg: Number?) = arg!!.toDouble() * degree
}