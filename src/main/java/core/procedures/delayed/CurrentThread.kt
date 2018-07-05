package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly

class CurrentThread : AFn<Nothing, Thread>(name = "current-thread", arity = Exactly(0)) {

    override operator fun invoke(): Thread = Thread.currentThread()
}