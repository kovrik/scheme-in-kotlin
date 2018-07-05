package core.procedures.time

import core.procedures.AFn
import core.procedures.Arity.Exactly

class NanoTime : AFn<Nothing, Any?>(name = "nano-time", arity = Exactly(0)) {

    override operator fun invoke() = System.nanoTime()
}