package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.InputPort

import java.io.FileInputStream
import java.io.FileNotFoundException

class OpenInputFile : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name = "open-input-file"

    override operator fun invoke(arg: Any?): Any {
        try {
            return InputPort(FileInputStream(arg.toString()))
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
    }
}
