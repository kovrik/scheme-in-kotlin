package core.procedures.time

import core.procedures.AFn
import core.procedures.Arity.Exactly

class CurrentMilliseconds : AFn<Nothing, Any?>(name = "current-milliseconds", arity = Exactly(0)) {

    override operator fun invoke() = System.currentTimeMillis()
}