package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.Thunk

class ForEach : AFn<Any?, Any>(name = "for-each", minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    private object map : MapProc() { override val name = "for-each" }
    private val void = VoidProc()

    /* For-each is the same as `map`, but ignores the result:
     * Void (ignore) results: (void <map-results>) */
    override operator fun invoke(args: Array<out Any?>) = Thunk(Cons.list(void, map(args)))
}