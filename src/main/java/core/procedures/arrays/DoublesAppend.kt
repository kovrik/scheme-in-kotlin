package core.procedures.arrays

import core.procedures.AFn

class DoublesAppend : AFn<Any?, DoubleArray>(name = "doubles-append", isPure = true, restArgsType = DoubleArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): DoubleArray {
        val doubles = mutableListOf<Double>()
        for (arr in args) {
            for (b in (arr as DoubleArray)) {
                doubles.add(b)
            }
        }
        return doubles.toDoubleArray()
    }
}
