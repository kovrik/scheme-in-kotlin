package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Hashmap

class HashMapToImmutableHashMap : AFn<Map<Any?, *>?, Hashmap<*, *>>(name = "hash-map->immutable-hash-map", isPure = true,
                                                                    arity = Exactly(1),
                                                                    mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<Any?, *>?) = when (arg) {
        is Hashmap -> arg
        else       -> Hashmap(arg!!)
    }
}
