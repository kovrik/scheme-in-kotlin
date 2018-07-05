package core.procedures.io

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.IPort

class ClosePort : AFn<IPort?, Unit>(name = "close-port", arity = Exactly(1),
                                    mandatoryArgsTypes = arrayOf(IPort::class.java)) {

    override operator fun invoke(arg: IPort?) = arg!!.close()
}
