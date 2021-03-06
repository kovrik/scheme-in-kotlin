package core.procedures.equivalence

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.MutableString

class Equal : AFn<Any?, Boolean>(name = "equal?", isPure = true, arity = AtLeast(2)) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        2    -> invoke(args[0], args[1])
        else -> (0..args.size - 2).all { invoke(args[it], args[it + 1]) }
    }

    override operator fun invoke(arg1: Any?, arg2: Any?) = when {
        arg1 is CharSequence && arg2 is MutableString -> arg2 == arg1
        // TODO Consider Clojure's = behaviour: (= [1 2] '(1 2)) ; => true
        // Utils.isSeqable(arg1) && Utils.isSeqable(arg2) -> Utils.toSequence(arg1).toList() == Utils.toSequence(arg2).toList()
        else -> arg1 == arg2
    }
}
