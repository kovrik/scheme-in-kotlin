package core.procedures.delayed

import core.procedures.AFn

class ThreadInterrupt : AFn<Thread, Unit>(name = "thread-interrupt", minArgs = 1, maxArgs = 1, lastArgType = Thread::class.java) {

    override operator fun invoke(arg: Thread) = arg.interrupt()
}