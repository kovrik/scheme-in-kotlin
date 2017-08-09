package core.procedures.symbols

import core.evaluator.Evaluator
import core.procedures.AFn
import core.scm.Symbol

open class Gensym : AFn<Any?, Symbol>(name = "gensym", isPure = true, minArgs = 0, maxArgs = 1,
                                      lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Symbol.intern("G__" + Evaluator.nextID())
        else -> Symbol.intern((args[0] as CharSequence).toString() + Evaluator.nextID())
    }
}
