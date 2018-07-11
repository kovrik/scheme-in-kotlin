package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.reader.FileReader
import core.scm.Thunk
import core.scm.specialforms.Begin
import java.io.File

class Load : AFn<CharSequence?, Any>(name = "load", arity = Exactly(1),
                                     mandatoryArgsTypes = arrayOf(CharSequence::class.java)) {

    private val reader = FileReader()

    override operator fun invoke(arg: CharSequence?) = Thunk(listOf(Begin).plus(reader.read(File(arg.toString()))))
}
