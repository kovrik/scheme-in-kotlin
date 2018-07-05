package core.procedures.cons

import core.procedures.AFn
import core.procedures.Arity.Exactly

class PairProc : AFn<Any?, Pair<*, *>>(name = "pair", arity = Exactly(2)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Pair(arg1, arg2)
}