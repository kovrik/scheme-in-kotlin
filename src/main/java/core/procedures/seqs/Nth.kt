package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.Type
import core.utils.Utils

class Nth : AFn<Any?, Any?>(name = "nth", isPure = true, arity = Range(2, 3)) {

    override operator fun invoke(args: Array<out Any?>): Any? {
        Type.assertType(name, args[1], Int::class.java)
        // val index = (args[1] as? Number)?.toInt() ?: throw WrongTypeException(name, "Integer", args[1])
        val index = (args[1]!! as Number).toInt()
        return args.first().let {
            when (it !is Sequence<*> && args.size < 3 && Utils.toSequence(it).count() <= index ) {
                true  -> throw IndexOutOfBoundsException("$name: value out of range: $index")
                false -> Utils.toSequence(it).elementAtOrElse(index) { args.getOrNull(2) }
            }
        }
    }
}
