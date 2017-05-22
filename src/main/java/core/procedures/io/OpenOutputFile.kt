package core.procedures.io

import core.exceptions.SCMFileNotFoundException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort

import java.io.FileNotFoundException
import java.io.FileOutputStream

class OpenOutputFile : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "open-output-file"

    override fun apply1(arg: Any?): Any {
        val filename = arg.toString()
        try {
            return OutputPort(FileOutputStream(filename))
        } catch (e: FileNotFoundException) {
            throw SCMFileNotFoundException(filename)
        }
    }
}
