package core.procedures.symbols

import core.Evaluator
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.Symbol

open class Gensym : AFn<Any?, Symbol>(name = "gensym", isPure = true, arity = Range(0, 1),
                                      lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Symbol.intern("G__" + Evaluator.nextID)
        else -> Symbol.intern((args[0] as CharSequence).toString() + Evaluator.nextID)
    }
}
