package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort
import java.io.FileNotFoundException
import java.io.FileOutputStream

class OpenOutputFile : AFn<Any?, Any>(name = "open-output-file", minArgs = 1, maxArgs = 1,
                                      mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?): Any {
        try {
            return OutputPort(FileOutputStream(arg!!.toString()))
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
    }
}
