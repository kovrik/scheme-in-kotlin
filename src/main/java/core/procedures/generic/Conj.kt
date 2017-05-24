package core.procedures.generic

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Cons
import core.scm.MutableVector
import core.scm.Vector

import java.util.*

class Conj : AFn(FnArgsBuilder().min(1).build()) {

    override val name: String
        get() = "conj"

    override fun apply(vararg args: Any?): Any? {
        val first = args[0]
        when {
            args.size == 1 -> return first
            first is List<*> -> {
                val list = Cons.list<Any?>()
                list.addAll(first)
                list.addAll(Arrays.asList(*args).subList(1, args.size))
                return list
            }
            first is Set<*> -> {
                val set = HashSet<Any?>()
                set.addAll(first as Collection<*>)
                set.addAll(Arrays.asList(*args).subList(1, args.size))
                return set
            }
            first is Vector -> {
                val size = first.size + args.size - 1
                val vector = MutableVector(size, null)
                val v = first
                val length = v.size
                for (i in 0..length - 1) {
                    vector.getArray()[i] = v[i]
                }
                System.arraycopy(args, 1, vector.getArray(), length, args.size - 1)
                return vector
            }
            // TODO Map?
            else -> throw WrongTypeException(name, "List or Vector or Set", first)
        }
    }
}
