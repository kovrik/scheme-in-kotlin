package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.InputPort

import java.io.IOException

class CloseInputPort : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(InputPort::class.java))) {

    override val name = "close-input-port"

    override operator fun invoke(arg: Any?) = try {
        (arg as InputPort).close()
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
