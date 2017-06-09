package core.procedures.io

import core.Repl
import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort
import core.writer.Writer

import java.io.IOException

class Write : AFn(name = "write", minArgs = 1, maxArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(Any::class.java), restArgsType = OutputPort::class.java) {

    override operator fun invoke(vararg args: Any?) = try {
        val outputPort: OutputPort = if (args.size == 1) Repl.currentOutputPort else args[1] as OutputPort
        outputPort.write(Writer.write(args[0]))
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
