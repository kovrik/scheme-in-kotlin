package core.procedures.seqs

import core.procedures.AFn
import core.utils.Utils

class Seq : AFn<Any?, Any?>(name = "seq", minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = Utils.toSequence(arg)
}
