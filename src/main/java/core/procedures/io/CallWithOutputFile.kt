package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.IFn
import core.scm.Cons
import core.scm.OutputPort
import core.scm.Thunk
import java.io.FileNotFoundException
import java.io.FileOutputStream

class CallWithOutputFile : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(CharSequence::class.java, IFn::class.java))) {

    override val name = "call-with-output-file"

    override operator fun invoke(vararg args: Any?): Any {
        val outputPort: OutputPort
        try {
            outputPort = OutputPort(FileOutputStream(args[0].toString()))
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
        val proc = args[1] as IFn<*, *>
        val sexp = Cons.list(proc, outputPort)
        return Thunk(sexp)
    }
}
