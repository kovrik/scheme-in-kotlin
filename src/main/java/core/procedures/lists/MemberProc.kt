package core.procedures.lists

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.Writer
import core.procedures.Arity.Exactly
import core.scm.Type
import core.utils.Utils

class MemberProc(override val name: String, private inline val predicate: AFn<Any?, Boolean>) :
        AFn<Any?, Any?>(isPure = true, arity = Exactly(2),
                        mandatoryArgsTypes = arrayOf(Any::class.java, Type.Seqable::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any {
        val search = when (arg1 is Sequence<*>) {
            true  -> arg1.toList()
            false -> arg1
        }
        when (arg2) {
            /* Special case */
            is Pair<*, *> -> {
                var p = 0
                var cur = arg2
                while (cur is Pair<*, *>) {
                    if (predicate(search, cur.first)) {
                        return cur
                    }
                    p += 1
                    cur = cur.second
                }
                throw WrongTypeException("$name: wrong type argument in position $p (expecting list): ${Writer.write(arg2)}")
            }
            /* Not found */
            else -> {
                var seq = Utils.toSequence(arg2)
                if (seq.none()) {
                    return false
                }
                var p = 0
                while (seq.any()) {
                    if (predicate(search, seq.first())) {
                        return seq
                    }
                    seq = seq.drop(1)
                    p += 1
                }
                return false
            }
        }
    }
}
