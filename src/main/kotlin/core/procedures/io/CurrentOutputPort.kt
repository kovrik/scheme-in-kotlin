package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.scm.OutputPort

class CurrentOutputPort : AFn<Nothing, OutputPort>(name = "current-output-port", maxArgs = 0) {

    override operator fun invoke() = Repl.currentOutputPort
}
