package core.procedures.cons

import core.procedures.AFn
import core.scm.ConsSeq

class ConsSeqProc : AFn<Any?, Sequence<*>>(name = "cons-seq", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = ConsSeq(arg1, arg2)
}