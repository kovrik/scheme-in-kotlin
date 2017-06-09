package core.procedures.io

import core.Repl
import core.procedures.AFn

class CurrentInputPort : AFn(name = "current-input-port", maxArgs = 0) {
    override operator fun invoke() = Repl.currentInputPort
}
