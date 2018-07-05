package core.scm

import core.procedures.AFn
import core.procedures.Arity.Exactly
import kotlin.collections.MutableSet

class MutableSet<T>(val set: MutableSet<T?>) : AFn<Any?, Any?>(arity = Exactly(1)), MutableSet<T?> by set {

    constructor() : this(mutableSetOf())

    constructor(size: Int) : this(HashSet(size))

    constructor(coll: Collection<T>) : this(HashSet(coll))

    override fun invoke(arg: Any?) = when (set.contains(arg)) {
        true -> arg
        else -> null
    }
}
