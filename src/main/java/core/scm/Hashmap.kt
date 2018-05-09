package core.scm

import core.exceptions.WrongTypeException
import core.procedures.AFn

class Hashmap<K, V>(val map: Map<K, V>) : AFn<K, Any?>(minArgs = 1, maxArgs = 2), Map<K, V> by map, IAssoc<K, V> {

    constructor() : this(mapOf())

    /* Maps are functions of their keys */
    override fun invoke(args: Array<out K>) = getOrDefault(args[0], args.getOrNull(1))

    override fun getEntry(key: K) = if (containsKey(key)) MapEntry(key, this[key]) else null

    override fun assoc(key: K, value: V) = throw WrongTypeException("assoc", "Mutable Hashmap", this)
}

