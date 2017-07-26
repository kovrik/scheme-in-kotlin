package core.procedures.io

import core.procedures.AFn
import core.reader.FileReader
import core.scm.Cons
import core.scm.Thunk
import core.scm.specialforms.Begin
import java.io.File

class Load : AFn<CharSequence?, Any>(name = "load", minArgs = 1, maxArgs = 1,
                                     mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    private val reader = FileReader()

    override operator fun invoke(arg: CharSequence?) = Thunk(Cons.list<Any>(Begin).apply {
        addAll(reader.read(File(arg.toString())))
    })
}
