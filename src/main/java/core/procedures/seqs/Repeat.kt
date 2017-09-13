package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class Repeat : AFn<Any?, Any?>(name = "repeat", minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> generateSequence(args[0], { it })
        else -> when {
            !Utils.isReal(args[0]) -> throw WrongTypeException(toString(), "Real", args[0])
            else -> generateSequence(args[1], { it }).take((args[0] as Number).toInt())
        }
    }
}