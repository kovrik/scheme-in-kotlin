package core.utils

class InternPool<T> {

    private val pool = WeakPool<T>()

    @Synchronized fun intern(obj: T): T {
        val result = pool[obj]
        return when (result) {
            null -> {
                pool.put(obj)
                obj
            }
            else -> result
        }
    }
}
