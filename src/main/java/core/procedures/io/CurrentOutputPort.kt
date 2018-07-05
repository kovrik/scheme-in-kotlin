package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.OutputPort

class CurrentOutputPort : AFn<Nothing, OutputPort>(name = "current-output-port", arity = Exactly(0)) {

    override operator fun invoke() = Repl.currentOutputPort
}
