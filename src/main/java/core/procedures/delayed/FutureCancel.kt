package core.procedures.delayed

import core.procedures.AFn

import java.util.concurrent.Future

class FutureCancel : AFn(name = "future-cancel", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Future::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as Future<*>).cancel(true)
}
