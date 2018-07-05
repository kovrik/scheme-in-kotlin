package core.scm

import core.procedures.AFn
import core.procedures.Arity.Range

class MutableHashmap<K, V>(val map: MutableMap<K, V>) : AFn<K, Any?>(arity = Range(1, 2)),
                                                        MutableMap<K, V> by map, IAssoc<K, V> {

    constructor() : this(mutableMapOf())
    constructor(size: Int) : this(LinkedHashMap(size))

    fun toImmutableMap() = Hashmap(this)

    /* Maps are functions of their keys */
    override fun invoke(args: Array<out K>) = getOrDefault(args[0], args.getOrNull(1))

    override fun getEntry(key: K) = if (containsKey(key)) MapEntry(key, this[key]) else null

    override fun assoc(key: K, value: V) = apply { put(key, value) }
}

