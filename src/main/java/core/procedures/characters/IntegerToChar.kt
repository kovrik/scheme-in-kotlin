package core.procedures.characters

import core.procedures.AFn
import core.procedures.Arity.Exactly

class IntegerToChar : AFn<Number?, Char>(name = "integer->char", arity = Exactly(1),
                                         mandatoryArgsTypes =  arrayOf(Long::class.javaObjectType)) {

    override operator fun invoke(arg: Number?) = arg!!.toChar()
}