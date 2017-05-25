package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.Vector
import core.utils.Utils

import java.util.*

open class Next : AFn(FnArgsBuilder().min(1).max(1).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "next"

    override operator fun invoke(arg: Any?): Any? {
        if (!Utils.isSeqable(arg)) {
            throw IllegalArgumentException("don't know how to create Sequence from " + arg!!.javaClass)
        }
        when (arg) {
            is List<*> -> {
                val list = arg
                return if (list.isEmpty()) null else list.subList(1, list.size)
            }
            is Set<*> -> return next(arg)
            is Map<*, *> -> return next(arg.entries)
            is Map.Entry<*, *> -> return Cons.list(arg.value!!)
            is Vector -> {
                val vec = arg
                return if (vec.size == 0) null else Vector(*Arrays.copyOfRange<Any>(vec.getArray(), 1, vec.size))
            }
            is CharSequence -> {
                val cs = arg
                return if (cs.length == 0) null else cs.subSequence(1, cs.length)
            }
            else -> throw WrongTypeException("next", "List or Vector or Set or String or Map", arg)
        }
    }

    private fun next(set: Set<*>): Any? {
        if (set.isEmpty()) {
            return null
        }
        val next = HashSet<Any?>()
        val iter = set.iterator()
        iter.next()
        while (iter.hasNext()) {
            next.add(iter.next())
        }
        return next
    }
}
