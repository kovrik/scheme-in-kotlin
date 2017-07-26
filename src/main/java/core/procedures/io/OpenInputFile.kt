package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.InputPort

import java.io.FileInputStream
import java.io.FileNotFoundException

class OpenInputFile : AFn<Any?, Any>(name = "open-input-file", minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = try {
        InputPort(FileInputStream(arg!!.toString()))
    } catch (e: FileNotFoundException) {
        throw ThrowableWrapper(e)
    }
}
