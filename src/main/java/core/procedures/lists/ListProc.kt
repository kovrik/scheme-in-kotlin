package core.procedures.lists

import core.procedures.AFn

class ListProc : AFn<Any?, List<*>>(name = "list", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = args.toList()
}
