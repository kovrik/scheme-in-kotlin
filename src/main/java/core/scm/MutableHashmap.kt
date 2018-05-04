package core.scm

import core.procedures.AFn

class MutableHashmap<K, V>(val map: MutableMap<K, V>) : AFn<K, Any?>(minArgs = 1, maxArgs = 2),
                                                        MutableMap<K, V> by map, IAssoc<K, V> {

    constructor() : this(mutableMapOf())
    constructor(size: Int) : this(LinkedHashMap(size))

    fun toImmutableMap() = Hashmap(this)

    /* Maps are functions of their keys */
    override fun invoke(args: Array<out K>) = map.getOrDefault(args[0], args.getOrNull(1))

    override fun getEntry(key: K) = if (map.containsKey(key)) MapEntry(key, map[key]) else null

    override fun assoc(key: K, value: V) = apply { put(key, value) }
}

