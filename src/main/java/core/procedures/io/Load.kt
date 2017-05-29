package core.procedures.io

import core.procedures.AFn
import core.procedures.FnArgs
import core.reader.FileReader
import core.scm.Cons
import core.scm.Thunk
import core.scm.specialforms.Begin
import java.io.File

class Load : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    private val reader = FileReader()

    override val name = "load"

    override operator fun invoke(arg: Any?): Any {
        val file = File(arg.toString())
        val sexps = Cons.list<Any>(Begin.BEGIN)
        sexps.addAll(reader.read(file))
        return Thunk(sexps)
    }
}
