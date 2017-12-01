package core.procedures.bytes

import core.procedures.AFn
import java.nio.charset.Charset

class BytesToString : AFn<Any?, String?>(name = "bytes->string", isPure = true, minArgs = 1, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf(ByteArray::class.java),
                                         lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0]?.let { String(args[0] as ByteArray) }
        else -> args[0]?.let { String(args[0] as ByteArray, Charset.forName((args[1] as CharSequence).toString())) }
    }
}
