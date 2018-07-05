package core.procedures.delayed

import core.procedures.AFn
import core.procedures.Arity.Exactly

class ThreadInterrupt : AFn<Thread, Unit>(name = "thread-interrupt", arity = Exactly(1), lastArgType = Thread::class.java) {

    override operator fun invoke(arg: Thread) = arg.interrupt()
}