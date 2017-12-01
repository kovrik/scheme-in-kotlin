package core.procedures.io

import core.procedures.AFn
import core.scm.OutputPort
import java.io.FileOutputStream

class OpenOutputFile : AFn<Any?, Any>(name = "open-output-file", minArgs = 1, maxArgs = 1,
                                      mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = OutputPort(FileOutputStream(arg!!.toString()))
}
