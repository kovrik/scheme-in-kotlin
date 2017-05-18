package core.scm

interface IAssoc {

    fun containsKey(key: Any): Boolean

    fun getEntry(key: Any): MapEntry?

    fun assoc(key: Any, value: Any): Any
}
