package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs
import core.writer.Writer

open class ToString : AFn(FnArgs(min = 0)) {

    override val isPure = true
    override val name = "->string"

    override operator fun invoke(vararg args: Any?): CharSequence? {
        return when {
            args.isEmpty() -> ""
            args.size == 1 -> str(args[0])
            else -> {
                val sb = StringBuilder()
                args.forEach { arg -> sb.append(str(arg)) }
                sb.toString()
            }
        }
    }

    private fun str(obj: Any?): CharSequence {
        return when (obj) {
            null -> ""
            is Char -> obj.toString()
            is CharSequence -> obj
            else -> Writer.write(obj)
        }
    }
}
