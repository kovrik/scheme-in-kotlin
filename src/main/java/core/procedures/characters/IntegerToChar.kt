package core.procedures.characters

import core.procedures.AFn

class IntegerToChar : AFn(name = "integer->char", minArgs = 1, maxArgs = 1,
                          mandatoryArgsTypes =  arrayOf<Class<*>>(Long::class.javaObjectType)) {

    override operator fun invoke(arg: Any?) = (arg as Number).toChar()
}