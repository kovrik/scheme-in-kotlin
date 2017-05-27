package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgsBuilder

class CurrentInputPort : AFn(FnArgsBuilder().max(0).build()) {

    override val name = "current-input-port"

    override operator fun invoke(): Any {
        return Repl.currentInputPort
    }
}
