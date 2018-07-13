package core.environment

import core.exceptions.UndefinedIdentifierException
import core.scm.Type

open class Environment : HashMap<Any?, Any?> {

    private val outer: Environment?

    constructor(size: Int, outer: Environment?) : super(size) {
        this.outer = outer
    }

    constructor(outer: Environment?) {
        this.outer = outer
    }

    fun resolve(key: Any?): Any? {
        var current = this
        while (true) {
            if (current.containsKey(key)) return current[key]
            if (current.outer == null) return Type.Undefined
            current = current.outer as Environment
        }
    }

    fun findAndSet(key: Any?, value: Any?): Any? {
        var current = this
        while (true) {
            if (current.containsKey(key)) return current.put(key, value)
            if (current.outer == null) throw UndefinedIdentifierException(key.toString())
            current = current.outer as Environment
        }
    }

    override fun toString() = "#<environment:" + (if (outer == null) "root" else super.toString()) + ">"
}
