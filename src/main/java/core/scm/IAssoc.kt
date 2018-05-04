package core.scm

interface IAssoc<K, V> {

    fun containsKey(key: K): Boolean

    fun getEntry(key: K): MapEntry<*, *>?

    fun assoc(key: K, value: V): Any
}
