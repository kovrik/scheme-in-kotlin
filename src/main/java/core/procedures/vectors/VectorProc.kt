package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorProc : AFn<Any?, MutableVector>(name = "vector") {

    override operator fun invoke(args: Array<out Any?>) = MutableVector(args)
}
