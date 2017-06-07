package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.OutputPort

import java.io.IOException

class CloseOutputPort : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(OutputPort::class.java))) {

    override val name = "close-output-port"

    override operator fun invoke(arg: Any?) = try {
        (arg as OutputPort).close()
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
