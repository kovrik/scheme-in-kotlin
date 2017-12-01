package core.procedures.characters

import core.procedures.AFn

class IntegerToChar : AFn<Number?, Char>(name = "integer->char", minArgs = 1, maxArgs = 1,
                                         mandatoryArgsTypes =  arrayOf(Long::class.javaObjectType)) {

    override operator fun invoke(arg: Number?) = arg!!.toChar()
}