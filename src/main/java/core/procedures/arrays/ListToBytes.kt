package core.procedures.arrays

import core.exceptions.WrongTypeException
import core.procedures.AFn
import core.utils.Utils

class ListToBytes : AFn<List<*>?, ByteArray?>(name = "list->bytes", isPure = true, minArgs = 1, maxArgs = 1,
                                              mandatoryArgsTypes = arrayOf<Class<*>>(List::class.java)) {

    override operator fun invoke(arg: List<*>?): ByteArray? {
        if (arg == null) return null
        arg.filterNot { Utils.isByte(it) }.forEach { throw WrongTypeException(name, "List of Bytes", arg) }
        return  (arg as? List<Byte>)?.toByteArray()
    }
}
