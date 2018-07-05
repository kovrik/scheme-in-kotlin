package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.InputPort

class CloseInputPort : AFn<InputPort?, Unit>(name = "close-input-port", arity = Exactly(1),
                                             mandatoryArgsTypes = arrayOf(InputPort::class.java)) {

    override operator fun invoke(arg: InputPort?) = arg!!.close()
}
