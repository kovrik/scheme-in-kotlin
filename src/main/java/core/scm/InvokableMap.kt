package core.scm

import core.procedures.AFn

// TODO generics, type safety
// TODO rename
class InvokableMap(val map: MutableMap<in Any?, Any?>) : AFn<Any?, Any?>(minArgs = 1, maxArgs = 2),
                                                         MutableMap<Any?, Any?> by map {

    constructor() : this(mutableMapOf())
    constructor(size: Int) : this(LinkedHashMap(size))

    /* Maps are functions of their keys */
    override fun invoke(vararg args: Any?) = when (args.size) {
        1    -> map[args[0]]
        else -> map.getOrDefault(args[0], args[1])
    }
}

