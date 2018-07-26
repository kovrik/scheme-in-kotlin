package core.utils

class InternPool<T> {

    private val pool = WeakPool<T>()

    @Synchronized
    fun intern(obj: T) = pool[obj].let {
        when (it) {
            null -> pool.put(obj).let { obj }
            else -> it
        }
    }
}
