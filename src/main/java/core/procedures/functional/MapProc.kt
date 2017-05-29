package core.procedures.functional

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.IFn
import core.procedures.generic.Count
import core.scm.Cons
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils
import java.util.*

class MapProc : AFn(FnArgsBuilder().min(2).mandatory(arrayOf<Class<*>>(IFn::class.java)).build()) {

    companion object {
        internal val MAP_PROC = MapProc()
    }

    private val count = Count()

    override val name = "map"

    // TODO Very naive implementation. Re-implement and optimize
    override operator fun invoke(vararg args: Any?): Thunk {
        /* Check that all lists/vectors are of the same size */
        val size = count(args[1])!!
        val iterators = HashMap<Int, Iterator<*>>(args.size - 1)
        for (i in 1..args.size - 1) {
            /* Check type */
            iterators.put(i, Utils.toSequence(args[i]))
            /* Check size */
            if (count(args[i]) != size) {
                throw IllegalArgumentException("$name: all collections must be of the same size")
            }
        }

        val lists = ArrayList<MutableList<Any?>>(size)
        for (i in 0..size - 1) {
            /* Add procedure as first element */
            lists.add(Cons.list(args[0]))
            /* Now add each Nth element of all lists */
            for (n in 1..args.size - 1) {
                val e = iterators[n]!!.next()
                if (e is List<*> || e is Symbol) {
                    lists[i].add(Quote.quote(e))
                } else {
                    lists[i].add(e)
                }
            }
        }
        val result = Cons.list<Any>(Symbol.intern("list"))
        result.addAll(lists)
        /* Return Thunk that will be evaluated and produce results */
        return Thunk(result)
    }
}
