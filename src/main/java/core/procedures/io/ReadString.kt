package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.InputPort
import core.scm.Type
import java.io.BufferedReader
import java.io.InputStreamReader

class ReadString : AFn<Any?, String>(name = "read-string", arity = Range(1, 2),
                                     mandatoryArgsTypes = arrayOf(Type.ExactNonNegativeInteger::class.java),
                                     restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>) = CharArray((args[0] as Number).toInt()).let {
        when (args.size) {
            1    -> BufferedReader(InputStreamReader(Repl.currentInputPort.inputStream))
            else -> BufferedReader(InputStreamReader((args[0] as InputPort).inputStream))
        }.read(it, 0, (args[0] as Number).toInt())
        String(it)
    }
}