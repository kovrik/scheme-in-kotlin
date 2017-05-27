package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgsBuilder

import java.util.concurrent.Future

class FutureCancel : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Future::class.java)).build()) {

    override val name = "future-cancel"

    override operator fun invoke(arg: Any?): Boolean {
        return (arg as Future<*>).cancel(true)
    }
}
