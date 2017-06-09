package core.procedures.strings

import core.procedures.AFn
import core.utils.Utils

class Join : AFn(name = "join", isPure = true, minArgs = 1, maxArgs = 2,
                 mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(vararg args: Any?): String? {
        if (args.size == 1) {
            return args[0].toString()
        }
        val separator = args[0].toString()
        val iterator = Utils.toSequence(args[1])
        if (!iterator.hasNext()) {
            return ""
        }
        val sb = StringBuilder()
        while (iterator.hasNext()) {
            sb.append(iterator.next())
            if (iterator.hasNext()) {
                sb.append(separator)
            }
        }
        return sb.toString()
    }
}
