package core.procedures.bytes

import core.procedures.AFn
import java.nio.charset.Charset

class StringToBytes : AFn<CharSequence?, ByteArray?>(name = "string->bytes", isPure = true, minArgs = 1, maxArgs = 1,
                                                     mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg?.toString()?.toByteArray(Charset.forName("UTF-8"))
}