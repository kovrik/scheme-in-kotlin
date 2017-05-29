package core.procedures.functional

import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.IFn
import core.scm.Cons
import core.scm.Thunk
import core.scm.Type

class ForEach : AFn(FnArgs(min = 2, mandatory = arrayOf<Class<*>>(IFn::class.java), rest = Type.ProperList::class.java)) {

    override val name = "for-each"

    override operator fun invoke(vararg args: Any?): Thunk {
        /* For-each is the same as Map, but ignores the result */
        val result = MapProc.MAP_PROC(*args)
        /* Void (ignore) results: (void <map-results>) */
        return Thunk(Cons.list(VoidProc.Companion.VOID, result))
    }
}