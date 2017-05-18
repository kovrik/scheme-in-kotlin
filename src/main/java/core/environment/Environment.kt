package core.environment

import core.exceptions.UndefinedIdentifierException

import java.util.*

open class Environment : HashMap<Any?, Any?> {

    /* Value for undefined identifiers. Required to distinguish undefined and nil bindings */
    companion object { val UNDEFINED = Any() }

    private val outer: Environment?

    constructor(size: Int, outer: Environment?) : super(size) {
        this.outer = outer
    }

    constructor(outer: Environment?) {
        this.outer = outer
    }

    fun findOrDefault(key: Any?, defaultValue: Any?): Any? {
        var current = this
        while (true) {
            if (current.containsKey(key)) return current[key]
            if (current.outer == null) return defaultValue
            current = current.outer as Environment
        }
    }

    fun findAndPut(key: Any?, value: Any?): Any? {
        var current = this
        while (true) {
            if (current.containsKey(key)) return current.put(key, value)
            if (current.outer == null) throw UndefinedIdentifierException(key.toString())
            current = current.outer as Environment
        }
    }

    override fun toString(): String {
        return "#<environment:" + (if (outer == null) "root" else super.toString()) + ">"
    }
}
