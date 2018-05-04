package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Hashmap

class HashMapToImmutableHashMap : AFn<Map<Any?, *>?, Hashmap<*, *>>(name = "hash-map->immutable-hash-map", isPure = true,
                                                                    minArgs = 1, maxArgs = 1,
                                                                    mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<Any?, *>?) = when (arg) {
        is Hashmap -> arg
        else       -> Hashmap(arg!!)
    }
}
