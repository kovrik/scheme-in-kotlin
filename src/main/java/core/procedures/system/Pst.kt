package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Pst : AFn<Throwable?, Unit>(name = "pst", arity = Exactly(1), mandatoryArgsTypes = arrayOf(Throwable::class.java)) {

    override operator fun invoke(arg: Throwable?) = arg!!.printStackTrace()
}
