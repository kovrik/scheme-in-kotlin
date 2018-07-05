package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.procedures.IFn
import core.scm.InputPort
import core.scm.Thunk
import core.scm.specialforms.Try

import java.io.FileInputStream

class CallWithInputFile : AFn<Any?, Any>(name = "call-with-input-file", arity = Exactly(2),
                                         mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    /* (try (proc in) (finally (close-input-port in)))*/
    override operator fun invoke(arg1: Any?, arg2: Any?) = InputPort(FileInputStream(arg1!!.toString())).let {
        Thunk(listOf(Try, listOf(arg2, it),
              listOf(Try.FINALLY, listOf(CloseInputPort(), it))))
    }
}
