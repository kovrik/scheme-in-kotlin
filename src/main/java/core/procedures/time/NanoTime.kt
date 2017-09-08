package core.procedures.time

import core.procedures.AFn

class NanoTime : AFn<Nothing, Any?>(name = "nano-time", maxArgs = 0) {

    override operator fun invoke() = System.nanoTime()
}