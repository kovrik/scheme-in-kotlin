package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.equivalence.Equal
import core.scm.Cons
import core.scm.Type
import core.utils.Utils
import core.writer.Writer

class AssocProc(override val name: String,
                /* Procedure used to compare objects for equality */
                private val predicate: AFn) : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(Any::class.java)).build()) {

    private val get = Get()

    override fun apply2(arg1: Any?, arg2: Any?): Any? {
        if (Type.checkType(arg2, Type.ProperList::class.java)) {
            val list = arg2 as List<*>?
            for (n in list!!.indices) {
                val pair = list[n]
                if (Cons.isPair(pair)) {
                    if (Utils.toBoolean(predicate.apply2(arg1, (pair as Cons<*>).car()))) {
                        return pair
                    }
                } else {
                    throw WrongTypeException(
                            String.format("%s: wrong type argument in position %s (expecting association list): %s", name, n, Writer.write(list)))
                }
            }
            return java.lang.Boolean.FALSE
        }
        if (predicate is Equal) {
            if (arg2 is Map<*, *>) {
                return get.apply3(arg2, arg1, null)
            }
            throw WrongTypeException(name, "List or Map", arg2)
        }
        throw WrongTypeException(name, "List", arg2)
    }

    override fun apply(args: Array<Any?>): Any? {
        return apply2(args[0], args[1])
    }
}
