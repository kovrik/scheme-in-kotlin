package core.procedures.characters

import core.procedures.AFn
import core.procedures.FnArgs

class IntegerToChar : AFn(FnArgs(min = 1, max = 1, mandatory =  arrayOf<Class<*>>(Long::class.javaObjectType))) {

    override val name = "integer->char"

    override operator fun invoke(arg: Any?): Char? {
        return (arg as Number).toChar()
    }
}