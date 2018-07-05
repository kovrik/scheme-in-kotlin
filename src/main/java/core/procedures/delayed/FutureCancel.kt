package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly

import java.util.concurrent.Future

class FutureCancel : AFn<Future<*>?, Boolean>(name = "future-cancel", arity = Exactly(1),
                                              mandatoryArgsTypes = arrayOf(Future::class.java)) {

    override operator fun invoke(arg: Future<*>?) = arg!!.cancel(true)
}
