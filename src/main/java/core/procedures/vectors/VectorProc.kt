package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorProc : AFn() {

    override val name: String
        get() = "vector"

    override operator fun invoke(vararg args: Any?): MutableVector {
        return MutableVector(*args)
    }
}
