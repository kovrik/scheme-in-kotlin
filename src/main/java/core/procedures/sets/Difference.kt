package core.procedures.sets

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.MutableSet

class Difference : AFn<Any?, Set<*>>(name = "difference", isPure = true, arity = AtLeast(1),
                                     mandatoryArgsTypes = arrayOf(Set::class.java), restArgsType = Set::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0]!! as Set<*>
        else -> MutableSet(args[0]!! as Set<*>).apply {
            for (i in (1 until args.size)) {
                removeAll(args[i]!! as Set<*>)
            }
        }
    }
}
