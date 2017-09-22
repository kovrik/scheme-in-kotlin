package core.procedures.cons

import core.procedures.AFn

class PairProc : AFn<Any?, Pair<*, *>>(name = "pair", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Pair(arg1, arg2)
}