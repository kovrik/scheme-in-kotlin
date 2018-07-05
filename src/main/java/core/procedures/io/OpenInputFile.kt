package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.InputPort

import java.io.FileInputStream

class OpenInputFile : AFn<Any?, Any>(name = "open-input-file", arity = Exactly(1),
                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = InputPort(FileInputStream(arg!!.toString()))
}
