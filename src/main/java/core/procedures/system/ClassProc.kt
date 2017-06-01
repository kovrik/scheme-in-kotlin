package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

open class ClassProc : AFn(FnArgs(min = 1, max = 1)) {

    override val isPure = true
    override val name = "class"
    override operator fun invoke(arg: Any?) = arg?.javaClass
}
