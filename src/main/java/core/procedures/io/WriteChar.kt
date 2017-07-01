package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort

import java.io.IOException

class WriteChar : AFn<Any?, Unit>(name = "write-char", minArgs = 1, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf<Class<*>>(Char::class.javaObjectType), restArgsType = OutputPort::class.java) {

    override operator fun invoke(args: Array<Any?>) = try {
        val outputPort: OutputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        outputPort.write((args[0] as Char).toInt())
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
