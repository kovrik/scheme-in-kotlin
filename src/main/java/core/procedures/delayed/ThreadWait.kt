package core.procedures.delayed

import core.procedures.AFn

class ThreadWait : AFn<Thread, Unit>(name = "thread-wait", minArgs = 1, maxArgs = 1, lastArgType = Thread::class.java) {

    override operator fun invoke(arg: Thread) = arg.join()
}