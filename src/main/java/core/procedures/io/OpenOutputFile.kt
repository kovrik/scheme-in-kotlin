package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort

import java.io.FileNotFoundException
import java.io.FileOutputStream

class OpenOutputFile : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name = "open-output-file"

    override operator fun invoke(arg: Any?): Any {
        try {
            return OutputPort(FileOutputStream(arg.toString()))
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
    }
}
