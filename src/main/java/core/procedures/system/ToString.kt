package core.procedures.system

import core.procedures.AFn
import core.writer.Writer

open class ToString : AFn<Any?, CharSequence>(name = "->string", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> ""
        1    -> str(args[0])
        else -> StringBuilder().apply { args.forEach { append(str(it)) } }.toString()
    }

    private fun str(obj: Any?) = when (obj) {
        null            -> ""
        is Char         -> obj.toString()
        is CharSequence -> obj
        else            -> Writer.write(obj)
    }
}
