package core.procedures.collections

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.equivalence.Equal
import core.procedures.predicates.Predicate
import core.procedures.seqs.Get
import core.utils.Utils
import core.Writer
import core.procedures.Arity.AtLeast
import core.scm.MutableHashmap
import core.scm.MutableVector
import core.scm.Vector

class AssocProc(override val name: String,
                /* Procedure used to compare objects for equality */
                private inline val predicate: AFn<Any?, Boolean>) :
        AFn<Any?, Any?>(arity = AtLeast(2), mandatoryArgsTypes = arrayOf(Any::class.java)) {

    private val car = Car()
    private val get = Get()

    override operator fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 2) {
            /* Scheme version of assoc */
            if (Predicate.isProperList(args[1])) {
                val list = args[1] as List<*>?
                for (n in list!!.indices) {
                    val pair = list[n]
                    when {
                        Predicate.isPair(pair) -> if (Utils.toBoolean(predicate(args[0], car(pair)))) {
                            return pair
                        }
                        else -> throw WrongTypeException("$name: wrong type argument in position $n (expecting association list): ${Writer.write(list)}")
                    }
                }
                return false
            }
            if (predicate is Equal) {
                if (args[1] is Map<*, *>) {
                    return get(args[1], args[0], null)
                }
                throw WrongTypeException(name, "List or Map", args[1])
            }
            throw WrongTypeException(name, "List", args[1])
        } else {
            /* Clojure version of assoc */
            if ((args.size % 2) != 1) throw IllegalArgumentException("$name expects even number of arguments after map/vector, found odd number")
            when (args[0]) {
                is Map<*, *> -> {
                    val argMap = args[0] as Map<*, *>
                    return MutableHashmap<Any?, Any?>(argMap.size).apply { putAll(argMap) }.apply {
                        for (i in 1 until args.size step 2) {
                            put(args[i], args[i + 1])
                        }
                    }
                }
                is Vector -> return MutableVector(args[0] as Vector).apply {
                    for (i in 1 until args.size step 2) {
                        val index = args[i] as? Number ?: throw WrongTypeException(name, "Integer", args[i])
                        set(index.toInt(), args[i + 1])
                    }
                }
                else -> throw WrongTypeException(name, "Map or Mutable Vector", args[0])
            }
        }
    }
}
