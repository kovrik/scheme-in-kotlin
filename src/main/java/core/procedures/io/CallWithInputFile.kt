package core.procedures.io

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons.Companion.list
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Try

import java.io.FileInputStream

class CallWithInputFile : AFn<Any?, Any>(name = "call-with-input-file", minArgs = 2, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    /* (try (proc in) (finally (close-input-port in)))*/
    override operator fun invoke(arg1: Any?, arg2: Any?) = InputPort(FileInputStream(arg1!!.toString())).let {
        Thunk(list(Try, list(arg2, it),
              list(Try.FINALLY, list(CloseInputPort(), it))))
    }
}
