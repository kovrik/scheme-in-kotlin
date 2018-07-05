package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.MutableVector
import core.scm.Type

class MakeVector : AFn<Any?, MutableVector>(name = "make-vector", isPure = true, arity = Range(1, 2),
                       mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = MutableVector((args[0] as Number).toInt(), args.getOrNull(1))
}
