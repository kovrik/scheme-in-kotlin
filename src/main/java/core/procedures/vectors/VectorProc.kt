package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorProc : AFn<Any?, MutableVector>(name = "vector") {

    override operator fun invoke(vararg args: Any?) = MutableVector(*args)
}
