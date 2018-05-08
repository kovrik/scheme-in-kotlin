package core.procedures.collections

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.equivalence.Equal
import core.procedures.predicates.Predicate
import core.procedures.seqs.Get
import core.utils.Utils
import core.Writer

class AssocProc(override val name: String,
                /* Procedure used to compare objects for equality */
                private inline val predicate: AFn<Any?, Boolean>) :
        AFn<Any?, Any?>(minArgs = 2, maxArgs = 2, mandatoryArgsTypes = arrayOf(Any::class.java)) {

    private val car = Car()
    private val get = Get()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        if (Predicate.isProperList(arg2)) {
            val list = arg2 as List<*>?
            for (n in list!!.indices) {
                val pair = list[n]
                if (Predicate.isPair(pair)) {
                    if (Utils.toBoolean(predicate(arg1, car(pair)))) {
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

    override operator fun invoke(args: Array<out Any?>) = invoke(args[0], args[1])
}
