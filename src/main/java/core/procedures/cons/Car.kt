package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class Car : AFn<Any?, Any?>(name = "car", isPure = true, arity = Exactly(1),
                            mandatoryArgsTypes = arrayOf(Type.PairOrNonEmptyList::class.java)) {

    override operator fun invoke(arg: Any?): Any? {
        val seq = Utils.toSequence(arg)
        when (seq.none()) {
            true -> throw WrongTypeException(name, Type.PairOrNonEmptyList::class.java, emptyList<Nothing>())
            false -> return seq.first()
        }
    }
}
