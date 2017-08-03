package core.procedures.io

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.OutputPort
import core.scm.Thunk
import core.scm.specialforms.Try

import java.io.FileOutputStream

class CallWithOutputFile : AFn<Any?, Any>(name = "call-with-output-file", minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Thunk {
        /* (try (proc out) (finally (close-output-port out)))*/
        val outputPort = OutputPort(FileOutputStream(arg1!!.toString()))
        val bodyBlock = Cons.list(arg2, outputPort)
        val finallyBlock = Cons.list(Try.FINALLY, Cons.list(CloseOutputPort(), outputPort))
        return Thunk(Cons.list(Try, bodyBlock, finallyBlock))
    }
}
