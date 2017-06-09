package core.procedures.io

import core.Repl
import core.procedures.AFn

class CurrentOutputPort : AFn(name = "current-output-port", maxArgs = 0) {
    override operator fun invoke() = Repl.currentOutputPort
}
