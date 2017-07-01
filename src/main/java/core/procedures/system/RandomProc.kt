package core.procedures.system

import core.exceptions.WrongTypeException
import core.procedures.AFn

import java.util.concurrent.ThreadLocalRandom

class RandomProc : AFn<Any?, Number>(name = "random", isPure = true, maxArgs = 2, restArgsType = Long::class.java) {

    override operator fun invoke(args: Array<Any?>): Number {
        if (args.isEmpty()) {
            return Math.random()
        }
        if (args.size == 1) {
            if (args[0] !is Long || (args[0] as Long) < 1 || args[0] as Long > Int.MAX_VALUE) {
                throw WrongTypeException(name, "Integer (1 to ${Int.MAX_VALUE})", args[0])
            }
            return java.util.Random().nextInt((args[0] as Long).toInt()).toLong()
        }
        if (args[0] !is Long || (args[0] as Long) < 1 || args[0] as Long > Int.MAX_VALUE) {
            throw WrongTypeException(name, "Integer (1 to ${Int.MAX_VALUE})", args[0])
        }
        if (args[1] !is Long || (args[1] as Long) < 1 || args[1] as Long > Int.MAX_VALUE) {
            throw WrongTypeException(name, "Integer (1 to ${Int.MAX_VALUE})", args[1])
        }
        return ThreadLocalRandom.current().nextInt((args[0] as Long).toInt(), (args[1] as Long).toInt()).toLong()
    }
}
