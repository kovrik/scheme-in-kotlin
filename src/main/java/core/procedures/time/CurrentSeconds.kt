package core.procedures.time

import core.procedures.AFn
import java.util.concurrent.TimeUnit

class CurrentSeconds : AFn<Nothing, Any?>(name = "current-seconds", maxArgs = 0) {

    override operator fun invoke() = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis())
}