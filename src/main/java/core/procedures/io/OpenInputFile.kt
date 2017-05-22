package core.procedures.io

import core.exceptions.SCMFileNotFoundException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.InputPort

import java.io.FileInputStream
import java.io.FileNotFoundException

class OpenInputFile : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "open-input-file"

    override fun apply1(arg: Any?): Any {
        val filename = arg.toString()
        try {
            return InputPort(FileInputStream(filename))
        } catch (e: FileNotFoundException) {
            throw SCMFileNotFoundException(filename)
        }

    }
}
