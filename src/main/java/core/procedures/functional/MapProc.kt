package core.procedures.functional

import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils

open class MapProc : AFn<Any?, Any>(name = "map", minArgs = 2, mandatoryArgsTypes = arrayOf<Class<*>>(IFn::class.java)) {

    // TODO Make it work with Sequences and return a Sequence
    // TODO Very naive implementation. Re-implement and optimize
    override operator fun invoke(args: Array<out Any?>): Thunk {
        val iterators = (1 until args.size).map { Utils.toSequence(args[it]).iterator() }
        val lists = mutableListOf<MutableList<Any?>>()
        while (iterators.all(Iterator<Any?>::hasNext)) {
            lists.add(Cons.list(args[0]).apply {
                addAll(iterators.map {
                    it.next().let {
                        when (it) {
                            is List<*>, is Symbol -> Quote.quote(it)
                            else -> it
                        }
                    }
                }.toList())
            })
        }
        /* Return Thunk that will be evaluated and produce results */
        return Thunk(Cons.list<Any>(Symbol.intern("list")).apply { addAll(lists)})
    }
}
