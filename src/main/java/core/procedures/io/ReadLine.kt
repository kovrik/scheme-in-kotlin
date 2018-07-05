package core.procedures.io

import core.Repl
import core.procedures.AFn
import core.procedures.Arity.Range
import core.scm.InputPort
import java.io.BufferedReader
import java.io.InputStreamReader

class ReadLine : AFn<Any?, String>(name = "read-line", arity = Range(0, 1), restArgsType = InputPort::class.java) {

    override operator fun invoke(args: Array<out Any?>): String = when (args.size) {
        0    -> BufferedReader(InputStreamReader(Repl.currentInputPort.inputStream))
        else -> BufferedReader(InputStreamReader((args[0] as InputPort).inputStream))
    }.readLine()
}