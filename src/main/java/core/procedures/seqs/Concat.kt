package core.procedures.seqs

import core.procedures.AFn
import core.scm.Thunk
import core.utils.Utils

class Concat : AFn<Any?, Any?>(name = "concat") {

    override operator fun invoke(args: Array<out Any?>) = Thunk(args.map { Utils.toSequence(it) }.reduce { f, s -> f + s })
}