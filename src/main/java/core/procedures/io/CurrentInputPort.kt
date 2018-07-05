package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.InputPort

class CurrentInputPort : AFn<Nothing, InputPort>(name = "current-input-port", arity = Exactly(0)) {

    override operator fun invoke() = Repl.currentInputPort
}
