package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.writer.Writer

open class ToString : AFn(FnArgsBuilder().min(0).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "->string"

    override operator fun invoke(vararg args: Any?): CharSequence? {
        when {
            args.isEmpty() -> return ""
            args.size == 1 -> return str(args[0])
            else -> {
                val sb = StringBuilder()
                for (arg in args) {
                    sb.append(str(arg))
                }
                return sb.toString()
            }
        }
    }

    private fun str(obj: Any?): CharSequence {
        when (obj) {
            null -> return ""
            is Char -> return obj.toString()
            is CharSequence -> return obj
            else -> return Writer.write(obj)
        }
    }
}
