package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

open class Rest : AFn<Any?, Any?>(name = "rest", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = Utils.toSequence(arg).drop(1)
}
