package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.InputPort

import java.io.IOException

class CloseInputPort : AFn<InputPort?, Unit>(name = "close-input-port", minArgs = 1, maxArgs = 1,
                           mandatoryArgsTypes = arrayOf<Class<*>>(InputPort::class.java)) {

    override operator fun invoke(arg: InputPort?) = try {
        arg!!.close()
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
