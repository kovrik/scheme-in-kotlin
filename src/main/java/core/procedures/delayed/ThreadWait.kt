package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly

class ThreadWait : AFn<Thread, Unit>(name = "thread-wait", arity = Exactly(1), lastArgType = Thread::class.java) {

    override operator fun invoke(arg: Thread) = arg.join()
}