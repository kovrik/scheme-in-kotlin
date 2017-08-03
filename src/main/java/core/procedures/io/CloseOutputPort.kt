package core.procedures.io

import core.procedures.AFn
import core.scm.OutputPort

class CloseOutputPort : AFn<OutputPort?, Unit>(name = "close-output-port", minArgs = 1, maxArgs = 1,
                                               mandatoryArgsTypes = arrayOf<Class<*>>(OutputPort::class.java)) {

    override operator fun invoke(arg: OutputPort?) = arg!!.close()
}
