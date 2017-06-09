package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorProc : AFn(name = "vector") {

    override operator fun invoke(vararg args: Any?) = MutableVector(*args)
}
