package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.procedures.seqs.Count
import core.scm.Cons
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils

open class MapProc : AFn<Any?, Any>(name = "map", minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    private val count = Count()

    // TODO Make it work with Sequences!
    // TODO Very naive implementation. Re-implement and optimize
    override operator fun invoke(args: Array<out Any?>): Thunk {
        /* Check that all lists/vectors are of the same size */
        if (!Utils.isSeqable(args[1])) {
            throw IllegalArgumentException("don't know how to create Sequence from ${args[1]?.javaClass}")
        }
        val size = count(args[1])
        val iterators = HashMap<Int, Iterator<*>>(args.size - 1)
        for (i in 1 until args.size) {
            /* Check type */
            iterators.put(i, Utils.toSequence(args[i]).iterator())
            /* Check size */
            if (count(args[i]) != size) {
                throw IllegalArgumentException("$name: all collections must be of the same size")
            }
        }

        val lists = ArrayList<MutableList<Any?>>(size)
        for (i in 0 until size) {
            /* Add procedure as first element */
            lists.add(Cons.list(args[0]))
            /* Now add each Nth element of all lists */
            for (n in 1 until args.size) {
                val e = iterators[n]!!.next()
                if (e is List<*> || e is Symbol) {
                    lists[i].add(Quote.quote(e))
                } else {
                    lists[i].add(e)
                }
            }
        }
        /* Return Thunk that will be evaluated and produce results */
        return Thunk(Cons.list<Any>(Symbol.intern("list")).apply { addAll(lists)})
    }
}
