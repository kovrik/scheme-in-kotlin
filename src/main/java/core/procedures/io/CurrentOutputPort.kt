package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.FnArgs

class CurrentOutputPort : AFn(FnArgs(max = 0)) {

    override val name = "current-output-port"
    override operator fun invoke() = Repl.currentOutputPort
}
