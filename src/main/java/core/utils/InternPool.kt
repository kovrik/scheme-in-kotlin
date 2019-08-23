package core.utils

class InternPool<T> {

    private val pool = WeakPool<T>()

    @Synchronized
    fun intern(obj: T) = when (val value = pool[obj]) {
        null -> pool.put(obj).let { obj }
        else -> value
    }
}
