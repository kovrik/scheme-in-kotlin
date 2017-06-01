package core.procedures.vectors

import core.procedures.AFn
import core.scm.Vector

class VectorImmutable : AFn() {

    override val name = "vector-immutable"
    override operator fun invoke(vararg args: Any?) = Vector(*args)
}
