package core.procedures.lists

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.procedures.cons.Car
import core.procedures.cons.Cdr
import core.scm.Cons
import core.utils.Utils
import core.writer.Writer

class MemberProc(override val name: String, /* Procedure used to compare objects for equality */
                 private val predicate: AFn) : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Any::class.java, List::class.java)).build()) {

    override val isPure = true

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        val list = arg2 as List<*>?
        if (list!!.isEmpty()) {
            return false
        }
        var p = 0
        var cons: Any? = list
        while (cons is List<*> && !cons.isEmpty()) {
            p += 1
            val car = Car.car(cons)
            if (Utils.toBoolean(predicate(arg1, car))) {
                return cons
            }
            cons = Cdr.cdr(cons)
        }
        /* Not found */
        if (p == list.size) {
            if (!Cons.isList(list)) {
                throw WrongTypeException("$name: wrong type argument in position $p (expecting list): ${Writer.write(list)}")
            }
            return false
        }
        throw WrongTypeException("$name: wrong type argument in position ${p+1} (expecting list): ${Writer.write(list)}")
    }
}
