package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorProc : AFn() {

    override val name: String
        get() = "vector"

    override fun apply(args: Array<Any?>): MutableVector {
        return MutableVector(*args)
    }
}