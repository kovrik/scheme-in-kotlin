package core.procedures.io

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Try

import java.io.FileInputStream

class CallWithInputFile : AFn<Any?, Any>(name = "call-with-input-file", minArgs = 2, maxArgs = 2,
                              mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Thunk {
        /* (try (proc in) (finally (close-input-port in)))*/
        val inputPort = InputPort(FileInputStream(arg1!!.toString()))
        val bodyBlock = Cons.list(arg2, inputPort)
        val finallyBlock = Cons.list(Try.FINALLY, Cons.list(CloseInputPort(), inputPort))
        return Thunk(Cons.list(Try, bodyBlock, finallyBlock))
    }
}
