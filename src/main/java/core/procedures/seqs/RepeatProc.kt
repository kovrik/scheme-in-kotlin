package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Repeat
import core.utils.Utils

class RepeatProc : AFn<Any?, Any?>(name = "repeat", minArgs = 1, maxArgs = 2) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> Repeat(args[0])
        else -> when {
            !Utils.isReal(args[0]) -> throw WrongTypeException(name, "Real", args[0])
            else -> Repeat(args[1]).take((args[0] as Number).toInt())
        }
    }
}