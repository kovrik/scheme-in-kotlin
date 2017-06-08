package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgs
import core.procedures.equivalence.Equal
import core.scm.Cons
import core.utils.Utils
import core.writer.Writer

class AssocProc(override val name: String,
                /* Procedure used to compare objects for equality */
                private val predicate: AFn) : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(Any::class.java))) {

    private val get = Get()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        if (Cons.Companion.isProperList(arg2)) {
            val list = arg2 as List<*>?
            for (n in list!!.indices) {
                val pair = list[n]
                if (Cons.isPair(pair)) {
                    if (Utils.toBoolean(predicate(arg1, (pair as Cons<*>).car()))) {
                        return pair
                    }
                } else {
                    throw WrongTypeException(
                        "$name: wrong type argument in position $n (expecting association list): ${Writer.write(list)}")
                }
            }
            return false
        }
        if (predicate is Equal) {
            if (arg2 is Map<*, *>) {
                return get(arg2, arg1, null)
            }
            throw WrongTypeException(name, "List or Map", arg2)
        }
        throw WrongTypeException(name, "List", arg2)
    }

    override operator fun invoke(vararg args: Any?) = invoke(args[0], args[1])
}
