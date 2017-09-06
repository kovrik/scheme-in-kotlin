package core.procedures.delayed

import core.procedures.AFn

class CurrentThread : AFn<Nothing, Thread>(name = "current-thread", minArgs = 0, maxArgs = 0) {

    override operator fun invoke(): Thread = Thread.currentThread()
}