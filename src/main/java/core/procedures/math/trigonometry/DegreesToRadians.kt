package core.procedures.math.trigonometry

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type

class DegreesToRadians : AFn<Number?, Number>(name = "degrees->radians", isPure = true, arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(Type.Real::class.java)) {

    override operator fun invoke(arg: Number?) = Math.toRadians(arg!!.toDouble())
}