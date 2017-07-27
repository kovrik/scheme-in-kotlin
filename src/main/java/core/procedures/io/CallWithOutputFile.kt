package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.OutputPort
import core.scm.Thunk
import java.io.FileNotFoundException
import java.io.FileOutputStream

class CallWithOutputFile : AFn<Any?, Any>(name = "call-with-output-file", minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any = try {
        Thunk(Cons.list(arg2, OutputPort(FileOutputStream(arg1!!.toString()))))
    } catch (e: FileNotFoundException) {
        throw ThrowableWrapper(e)
    }
}
