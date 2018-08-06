package core.procedures.functional

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.procedures.IFn
import core.scm.Symbol
import core.scm.Thunk
import core.scm.specialforms.Quote
import core.utils.Utils

open class MapProc : AFn<Any?, Thunk<*>>(name = "map", arity = AtLeast(2), mandatoryArgsTypes = arrayOf(IFn::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2 -> Thunk(object : Sequence<Any?> {
            override fun iterator(): Iterator<Any?> = object : Iterator<Any?> {

                private val fn = args[0] as? IFn<*, *> ?: throw WrongTypeException(name, "Procedure", args[0])
                private val iterator = Utils.toSequence(args[1]).iterator()

                override fun hasNext() = iterator.hasNext()

                override fun next() = AFn.invokeN<Any?, Any?>(fn, arrayOf(iterator.next())).let {
                    when (it) {
                        is List<*>, is Symbol -> Quote.quote(it)
                        else -> it
                    }
                }
            }
        })
        else -> Thunk(object : Sequence<Any?> {
            override fun iterator(): Iterator<Any?> = object : Iterator<Any?> {

                private val fn = args[0] as? IFn<*, *> ?: throw WrongTypeException(name, "Procedure", args[0])
                private val iterators = (1 until args.size).map { Utils.toSequence(args[it]).iterator() }

                override fun hasNext() = iterators.all { it.hasNext() }

                override fun next() = AFn.invokeN<Any?, Any?>(fn, iterators.map { it.next() }.toTypedArray()).let {
                    when (it) {
                        is List<*>, is Symbol -> Quote.quote(it)
                        else -> it
                    }
                }
            }
        })
    }
}
