package core.procedures.io

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons.Companion.list
import core.scm.OutputPort
import core.scm.Thunk
import core.scm.specialforms.Try

import java.io.FileOutputStream

class CallWithOutputFile : AFn<Any?, Any>(name = "call-with-output-file", minArgs = 2, maxArgs = 2,
                                          mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    /* (try (proc out) (finally (close-output-port out)))*/
    override operator fun invoke(arg1: Any?, arg2: Any?) = OutputPort(FileOutputStream(arg1!!.toString())).let {
        Thunk(list(Try, list(arg2, it),
              list(Try.FINALLY, list(CloseOutputPort(), it))))
    }
}
