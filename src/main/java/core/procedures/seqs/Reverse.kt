package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Reverse : AFn<Any?, Any?>(name = "reverse", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?): Any? = Utils.toSequence(arg).toList().reversed()
}
