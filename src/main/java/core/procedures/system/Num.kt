package core.procedures.system

import core.procedures.AFn

class Num : AFn<Any?, Number>(name = "num", isPure = true, minArgs = 1, maxArgs = 1) {

    override operator fun invoke(arg: Any?) = arg!! as Number
}
