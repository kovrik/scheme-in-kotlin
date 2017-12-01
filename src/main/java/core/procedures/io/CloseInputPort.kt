package core.procedures.io

import core.procedures.AFn
import core.scm.InputPort

class CloseInputPort : AFn<InputPort?, Unit>(name = "close-input-port", minArgs = 1, maxArgs = 1,
                                             mandatoryArgsTypes = arrayOf(InputPort::class.java)) {

    override operator fun invoke(arg: InputPort?) = arg!!.close()
}
