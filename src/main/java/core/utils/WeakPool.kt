package core.utils

import java.lang.ref.WeakReference
import java.util.WeakHashMap

internal class WeakPool<T> {

    private val pool = WeakHashMap<T, WeakReference<T>>()

    operator fun get(obj: T) = pool[obj]?.get()

    fun put(obj: T) {
        pool.put(obj, WeakReference(obj))
    }
}
