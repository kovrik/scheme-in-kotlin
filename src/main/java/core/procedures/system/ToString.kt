package core.procedures.system

import core.procedures.AFn
import core.writer.Writer

open class ToString : AFn(name = "->string", isPure = true) {

    override operator fun invoke(vararg args: Any?) = when {
        args.isEmpty() -> ""
        args.size == 1 -> str(args[0])
        else -> {
            val sb = StringBuilder()
            args.forEach { arg -> sb.append(str(arg)) }
            sb.toString()
        }
    }

    private fun str(obj: Any?) = when (obj) {
        null            -> ""
        is Char         -> obj.toString()
        is CharSequence -> obj
        else            -> Writer.write(obj)
    }
}
