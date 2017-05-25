package core.procedures.io

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.InputPort
import core.scm.Void

import java.io.IOException

class CloseInputPort : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(InputPort::class.java)).build()) {

    override val name: String
        get() = "close-input-port"

    override operator fun invoke(arg: Any?): Void {
        try {
            (arg as InputPort).close()
        } catch (e: IOException) {
            e.printStackTrace()
        }
        return Void.VOID
    }
}
