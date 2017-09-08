package core.procedures.delayed

import core.exceptions.ArityException
import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.scm.IDeref
import core.utils.Utils

class Deref : AFn<Any?, Any?>(name = "deref", minArgs = 1, maxArgs = 3,
                              mandatoryArgsTypes = arrayOf<Class<*>>(IDeref::class.java)) {

    override fun invoke(args: Array<out Any?>): Any? {
        if (args.size == 1) {
            return (args[0]!! as IDeref).deref()
        }
        if (args.size == 3) {
            val timeout = args[1]
            if (!Utils.isReal(timeout)) {
                throw WrongTypeException(name, "Real", timeout)
            }
            return (args[0]!! as IDeref).deref((args[1] as Number).toLong(), args[2])
        }
        // TODO fix it, implement properly
        throw ArityException("$name: arity mismatch; the expected number of arguments does not match the given number" +
                             " (expected: 1 or 3, given: ${args.size})")
    }
}