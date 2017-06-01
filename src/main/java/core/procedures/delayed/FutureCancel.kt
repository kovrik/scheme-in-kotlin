package core.procedures.delayed

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.concurrent.Future

class FutureCancel : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Future::class.java))) {

    override val name = "future-cancel"

    override operator fun invoke(arg: Any?) = (arg as Future<*>).cancel(true)
}
