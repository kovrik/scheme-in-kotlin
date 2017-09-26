package core.procedures.cons

import core.procedures.AFn
import core.scm.MutablePair

class Mcons : AFn<Any?, MutablePair<*, *>>(name = "mcons", minArgs = 2, maxArgs = 2) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = MutablePair(arg1, arg2)
}