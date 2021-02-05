package core.procedures.seqs

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils
import java.util.*

class RandNth : AFn<Any?, Any?>(
    name = "rand-nth", isPure = true, arity = Exactly(1),
    mandatoryArgsTypes = arrayOf(Type.Seqable::class.java)
) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is Map<*, *> -> throw UnsupportedOperationException("$name: not supported on this type: ${arg.javaClass}")
        else -> Utils.toSequence(arg).let {
            when {
                it.none() -> throw IndexOutOfBoundsException()
                else -> it.elementAt(Random().nextInt(it.count()))
            }
        }
    }
}

