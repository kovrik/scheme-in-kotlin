package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgs

class CurrentInputPort : AFn(FnArgs(max = 0)) {

    override val name = "current-input-port"
    override operator fun invoke() = Repl.currentInputPort
}
