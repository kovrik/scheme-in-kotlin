package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.Type
import core.utils.Utils
import java.util.concurrent.ThreadLocalRandom

class RandProc : AFn<Any?, Number>(name = "rand", isPure = true, maxArgs = 2, restArgsType = Type.Real::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number = when (args.size) {
        0 -> Math.random()
        1 -> {
            val bound = (args[0] as Number).toDouble()
            if (Utils.isNegative(bound)) {
                throw WrongTypeException(name, "non-negative bound", args[0])
            }
            when (bound) {
                0.0  -> 0.0
                else -> ThreadLocalRandom.current().nextDouble(bound)
            }
        }
        else -> {
            val origin = (args[0] as Number).toDouble()
            if (Utils.isNegative(origin)) {
                throw WrongTypeException(name, "non-negative origin", args[0])
            }
            val bound  = (args[1] as Number).toDouble()
            if (Utils.isNegative(bound)) {
                throw WrongTypeException(name, "non-negative bound", args[1])
            }
            when (origin) {
                bound -> origin
                else  -> ThreadLocalRandom.current().nextDouble(origin, bound)
            }
        }
    }
}