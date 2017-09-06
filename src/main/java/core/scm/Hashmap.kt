package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn

// TODO generify
class Hashmap(val map: Map<in Any?, Any?>) : AFn<Any?, Any?>(minArgs = 1, maxArgs = 2),
                                             Map<Any?, Any?> by map, IAssoc {

    constructor() : this(mapOf())
    constructor(size: Int) : this(LinkedHashMap(size))

    fun toMutableMap() = this.toMap(MutableHashmap())

    /* Maps are functions of their keys */
    override fun invoke(args: Array<out Any?>) = map.getOrDefault(args[0], args.getOrNull(1))

    override fun getEntry(key: Any?) = if (map.containsKey(key)) MapEntry(key, map[key]) else null

    override fun assoc(key: Any?, value: Any?) = throw WrongTypeException("assoc", "Mutable Hashmap", this)
}

