package core.procedures.seqs

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.math.Ceiling
import core.scm.Type
import core.utils.Utils

class TakeLast : AFn<Any?, Any?>(name = "take-last", isPure = true, minArgs = 1, maxArgs = 2) {

    private val ceiling = Ceiling()

    override operator fun invoke(args: Array<out Any?>): Any? {
        val n = when (args.size) {
            2 -> when {
                !Utils.isReal(args[0]) -> throw WrongTypeException(name, Type.Real::class.java, args[0])
                else -> when (Utils.isInteger(args[0])) {
                    true  -> (args[0] as Number).toInt()
                    false -> ceiling(args[0] as Number).toInt()
                }
            }
            else -> 1
        }
        if (n <= 0) return null

        val seq = Utils.toSequence(args.last())
        val count = seq.count()
        val result = seq.filterIndexed { index, e -> count - index <= n }
        return when (result.iterator().hasNext()) {
            true  -> result
            false -> null
        }
    }
}
