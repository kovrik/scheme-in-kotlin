package core.procedures.io

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.IFn
import core.scm.Cons
import core.scm.InputPort
import core.scm.Thunk

import java.io.FileInputStream
import java.io.FileNotFoundException

class CallWithInputFile : AFn<Any?, Any>(name = "call-with-input-file", minArgs = 2, maxArgs = 2,
                              mandatoryArgsTypes = arrayOf(CharSequence::class.java, IFn::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any {
        val inputPort = try {
            InputPort(FileInputStream(arg1!!.toString()))
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
        return Thunk(Cons.list(arg2, inputPort))
    }
}
