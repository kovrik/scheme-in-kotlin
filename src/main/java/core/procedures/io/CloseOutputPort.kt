package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.OutputPort

class CloseOutputPort : AFn<OutputPort?, Unit>(name = "close-output-port", arity = Exactly(1),
                                               mandatoryArgsTypes = arrayOf(OutputPort::class.java)) {

    override operator fun invoke(arg: OutputPort?) = arg!!.close()
}
