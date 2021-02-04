package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

class RadiansToDegrees : AFn<Number?, Number>(name = "radians->degrees", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?) = Math.toDegrees(arg!!.toDouble())
}