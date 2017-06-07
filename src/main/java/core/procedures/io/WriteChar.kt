package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort

import java.io.IOException

class WriteChar : AFn(FnArgs(min = 1, max = 2, mandatory = arrayOf<Class<*>>(Char::class.javaObjectType), rest = OutputPort::class.java)) {

    override val name = "write-char"

    override operator fun invoke(vararg args: Any?) = try {
        val outputPort: OutputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        outputPort.write((args[0] as Char).toInt())
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
