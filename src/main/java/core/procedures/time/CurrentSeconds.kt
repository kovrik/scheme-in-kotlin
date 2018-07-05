package core.procedures.time

import core.procedures.AFn
import core.procedures.Arity.Exactly
import java.util.concurrent.TimeUnit

class CurrentSeconds : AFn<Nothing, Any?>(name = "current-seconds", arity = Exactly(0)) {

    override operator fun invoke() = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis())
}