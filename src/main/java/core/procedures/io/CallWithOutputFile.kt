package core.procedures.io

import core.exceptions.SCMFileNotFoundException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.IFn
import core.scm.Cons
import core.scm.OutputPort
import core.scm.Thunk
import java.io.FileNotFoundException
import java.io.FileOutputStream

class CallWithOutputFile : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(CharSequence::class.java, IFn::class.java)).build()) {

    override val name: String
        get() = "call-with-output-file"

    override fun apply(vararg args: Any?): Any {
        val filename = args[0].toString()
        val outputPort: OutputPort
        try {
            outputPort = OutputPort(FileOutputStream(filename))
        } catch (e: FileNotFoundException) {
            throw SCMFileNotFoundException(filename)
        }

        val proc = args[1] as IFn<*, *>
        val sexp = Cons.list(proc, outputPort)
        return Thunk(sexp)
    }
}
