package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

open class Count : AFn<Any?, Int>(name = "count", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg).count()
}
