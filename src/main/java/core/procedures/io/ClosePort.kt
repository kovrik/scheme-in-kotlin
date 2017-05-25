package core.procedures.io

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.IPort
import core.scm.Void

import java.io.IOException

class ClosePort : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(IPort::class.java)).build()) {

    override val name: String
        get() = "close-port"

    override operator fun invoke(arg: Any?): Void? {
        try {
            (arg as IPort).close()
        } catch (e: IOException) {
            e.printStackTrace()
        }
        return Void.VOID
    }
}
