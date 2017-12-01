package core.procedures.io

import core.procedures.AFn
import core.scm.IPort

class ClosePort : AFn<IPort?, Unit>(name = "close-port", minArgs = 1, maxArgs = 1,
                                    mandatoryArgsTypes = arrayOf(IPort::class.java)) {

    override operator fun invoke(arg: IPort?) = arg!!.close()
}
