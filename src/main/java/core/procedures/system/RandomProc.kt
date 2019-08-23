package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.Range
import java.util.*

import java.util.concurrent.ThreadLocalRandom

class RandomProc : AFn<Any?, Number>(name = "random", isPure = true, arity = Range(0, 2), restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<out Any?>): Number = when (args.size) {
        0 -> Math.random()
        1 -> {
            if (args[0] !is Long || (args[0] as Long) < 0 || args[0] as Long > Int.MAX_VALUE) {
                throw WrongTypeException(name, "Integer (0 to ${Int.MAX_VALUE})", args[0])
            }
            when (val bound = (args[0] as Long).toInt()) {
                0 -> 0
                else -> Random().nextInt(bound).toLong()
            }
        }
        else -> {
            if (args[0] !is Long || (args[0] as Long) < 0 || args[0] as Long > Int.MAX_VALUE) {
                throw WrongTypeException(name, "Integer (0 to ${Int.MAX_VALUE})", args[0])
            }
            if (args[1] !is Long || (args[1] as Long) < 0 || args[1] as Long > Int.MAX_VALUE) {
                throw WrongTypeException(name, "Integer (0 to ${Int.MAX_VALUE})", args[1])
            }
            val bound  = (args[1] as Long).toInt()
            when (val origin = (args[0] as Long).toInt()) {
                bound -> origin
                else  -> ThreadLocalRandom.current().nextInt(origin, bound).toLong()
            }
        }
    }
}
