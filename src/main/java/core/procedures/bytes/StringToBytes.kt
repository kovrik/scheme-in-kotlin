package core.procedures.bytes

import core.procedures.AFn
import java.nio.charset.Charset

class StringToBytes : AFn<Any?, ByteArray?>(name = "string->bytes", isPure = true, minArgs = 1, maxArgs = 2,
                                            mandatoryArgsTypes = arrayOf(CharSequence::class.java),
                                            lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = args[0]?.toString()?.toByteArray(
            Charset.forName((args.getOrElse(1, { "UTF-8" }) as CharSequence).toString())
    )
}