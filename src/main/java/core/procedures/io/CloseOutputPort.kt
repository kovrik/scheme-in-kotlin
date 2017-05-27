package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.OutputPort
import core.scm.Void

import java.io.IOException

class CloseOutputPort : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(OutputPort::class.java)).build()) {

    override val name = "close-output-port"

    override operator fun invoke(arg: Any?): Void {
        try {
            (arg as OutputPort).close()
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        }
        return Void
    }
}
