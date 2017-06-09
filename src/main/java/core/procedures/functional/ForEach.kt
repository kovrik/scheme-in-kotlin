package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.Thunk

class ForEach : AFn(name = "for-each", minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    object map : MapProc() { override val name = "for-each" }

    override operator fun invoke(vararg args: Any?): Thunk {
        /* For-each is the same as `map`, but ignores the result */
        val result = map.invoke(*args)
        /* Void (ignore) results: (void <map-results>) */
        return Thunk(Cons.list(VoidProc.VOID, result))
    }
}