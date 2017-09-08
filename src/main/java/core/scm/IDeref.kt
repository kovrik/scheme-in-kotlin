package core.scm

interface IDeref {

    fun deref(): Any?

    fun deref(timeout: Long, timeoutVal: Any?): Any?
}
