package core.procedures.time

import core.procedures.AFn

class CurrentMilliseconds : AFn<Nothing, Any?>(name = "current-milliseconds", maxArgs = 0) {

    override operator fun invoke() = System.currentTimeMillis()
}