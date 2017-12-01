package core.procedures.io

import core.procedures.AFn
import core.scm.InputPort

import java.io.FileInputStream

class OpenInputFile : AFn<Any?, Any>(name = "open-input-file", minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = InputPort(FileInputStream(arg!!.toString()))
}
