package core.scm

import core.procedures.AFn

// TODO generics, type safety
// TODO rename
class InvokableMap(val map: MutableMap<in Any?, Any?>) : AFn<Any?, Any?>(minArgs = 1, maxArgs = 2),
                                                         MutableMap<Any?, Any?> by map, IAssoc {

    constructor() : this(mutableMapOf())
    constructor(size: Int) : this(LinkedHashMap(size))

    /* Maps are functions of their keys */
    override fun invoke(args: Array<out Any?>) = map.getOrDefault(args[0], args.getOrNull(1))

    override fun getEntry(key: Any?) = if (map.containsKey(key)) MapEntry(key, map[key]) else null

    override fun assoc(key: Any?, value: Any?): Any {
        map.put(key, value)
        return this
    }

}

