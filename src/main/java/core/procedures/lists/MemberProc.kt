package core.procedures.lists

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.scm.Cons
import core.utils.Utils
import core.writer.Writer

class MemberProc(override val name: String, inline private val predicate: AFn<Any?, Boolean>) :
        AFn<Any?, Any?>(isPure = true, minArgs = 2, maxArgs = 2,
                        mandatoryArgsTypes = arrayOf(Any::class.java, List::class.java)) {

    private val car = Car()
    private val cdr = Cdr()

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg2 as List<*>?
        if (list!!.isEmpty()) {
            return false
        }
        var p = 0
        var cons: Any? = list
        while (cons is List<*> && !cons.isEmpty()) {
            p += 1
            if (Utils.toBoolean(predicate(arg1, car(cons)))) {
                return cons
            }
            cons = cdr(cons)
        }
        /* Not found */
        if (p == list.size) {
            if (!Cons.isProperList(list)) {
                throw WrongTypeException("$name: wrong type argument in position $p (expecting list): ${Writer.write(list)}")
            }
            return false
        }
        throw WrongTypeException("$name: wrong type argument in position ${p+1} (expecting list): ${Writer.write(list)}")
    }
}
