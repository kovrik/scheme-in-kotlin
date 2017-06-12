package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.IPort

import java.io.IOException

class ClosePort : AFn<IPort?, Unit>(name = "close-port", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(IPort::class.java)) {

    override operator fun invoke(arg: IPort?) = try {
        arg!!.close()
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
