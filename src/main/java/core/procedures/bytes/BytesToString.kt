package core.procedures.bytes

import core.procedures.AFn
import core.procedures.Arity.Range
import java.nio.charset.Charset

class BytesToString : AFn<Any?, String?>(name = "bytes->string", isPure = true, arity = Range(1, 2),
                                         mandatoryArgsTypes = arrayOf(ByteArray::class.java),
                                         lastArgType = CharSequence::class.java) {

    override operator fun invoke(args: Array<out Any?>) = args[0]?.let {
        String(args[0] as ByteArray, Charset.forName((args.getOrElse(1, { "UTF-8" }) as CharSequence).toString()))
    }
}
