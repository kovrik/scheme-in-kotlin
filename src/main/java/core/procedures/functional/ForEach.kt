package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Thunk

/* For-each is the same as `map`, but ignores the result: Void (ignore) results: (void <map-results>) */
class ForEach : AFn<Any?, Any?>(name = "for-each", minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    private object map : MapProc() { override val name = "for-each" }

    private val void = VoidProc()
    private val apply = Apply()

    override operator fun invoke(args: Array<out Any?>) = Thunk(listOf(apply, void, map(args)))
}