package core.procedures.seqs

import core.procedures.AFn

class Repeat : AFn<Any?, Any?>(name = "repeat", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = generateSequence(args[0], { it })
}