package core.procedures.bytes

import core.procedures.AFn
import java.nio.charset.Charset

class StringToBytes : AFn<Any?, ByteArray?>(name = "string->bytes", isPure = true, minArgs = 1, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java),
                                            lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        1    -> args[0]?.toString()?.toByteArray(Charsets.UTF_8)
        else -> args[0]?.toString()?.toByteArray(Charset.forName((args[1] as CharSequence).toString()))
    }
}