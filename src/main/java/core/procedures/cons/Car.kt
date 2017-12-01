package core.procedures.cons

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Type

class Car : AFn<Any?, Any?>(name = "car", isPure = true, minArgs = 1, maxArgs = 1,
                            mandatoryArgsTypes = arrayOf(Type.Pair::class.java)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Collection<*> -> when {
            arg.isEmpty() -> throw WrongTypeException("car", Type.Pair::class.java, emptyList<Nothing>())
            else -> arg.first()
        }
        is Pair<*, *> -> arg.first
        else -> (arg as List<*>)[0]
    }
}
