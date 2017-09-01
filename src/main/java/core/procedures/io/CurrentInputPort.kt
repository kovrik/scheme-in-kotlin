package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.InputPort

class CurrentInputPort : AFn<Nothing, InputPort>(name = "current-input-port", maxArgs = 0) {

    override operator fun invoke() = Repl.currentInputPort
}
