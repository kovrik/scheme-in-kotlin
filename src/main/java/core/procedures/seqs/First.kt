package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils.toSequence

class First : AFn<Any?, Any?>(name = "first", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = arg?.let {  toSequence(arg).firstOrNull() }
}
