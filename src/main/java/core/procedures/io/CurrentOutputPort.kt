package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class CurrentOutputPort : AFn(FnArgsBuilder().max(0).build()) {

    override val name: String
        get() = "current-output-port"

    override fun apply0(): Any {
        return Repl.currentOutputPort
    }
}
