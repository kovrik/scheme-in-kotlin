package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Num : AFn<Any?, Number>(name = "num", isPure = true, arity = Exactly(1)) {

    override operator fun invoke(arg: Any?) = arg!! as Number
}
