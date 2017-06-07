package core.procedures.hashmaps

import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MapEntry
import core.scm.MutableVector
import core.utils.Utils
import java.util.*

class Put : AFn(FnArgs(min = 3, mandatory = arrayOf<Class<*>>(Any::class.java, Any::class.java, Any::class.java))) {

    override val isPure = true
    override val name = "put"

    override operator fun invoke(vararg args: Any?): Any? {
        var m = args[0]
        if (m is Map<*, *>) {
            if (args.size % 2 != 1) {
                throw IllegalArgumentException(name + ": no value supplied for key: " + args[args.size - 1])
            }
            val map = HashMap(args[0] as Map<*, *>)
            var i = 1
            while (i < args.size) {
                map.put(args[i], args[i + 1])
                i += 2
            }
            return map
        }
        if (args.size > 3) {
            throw ArityException(name, 2, 2, args.size)
        }
        if (m is Map.Entry<*, *>) {
            m = MapEntry(m)
        }
        if (m is MutableVector) {
            if (!Utils.isInteger(args[1])) {
                throw WrongTypeException("vector", Int::class.java, args[1])
            }
            m[(args[1] as Number).toInt()] = args[2]
            return m
        }
        throw WrongTypeException(name, "MutableVector or Map or MapEntry", m)
    }
}
