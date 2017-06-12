package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.scm.OutputPort

import java.io.IOException

class CloseOutputPort : AFn<OutputPort?, Unit>(name = "close-output-port", minArgs = 1, maxArgs = 1,
                            mandatoryArgsTypes = arrayOf<Class<*>>(OutputPort::class.java)) {

    override operator fun invoke(arg: OutputPort?) = try {
        arg!!.close()
    } catch (e: IOException) {
        throw ThrowableWrapper(e)
    }
}
