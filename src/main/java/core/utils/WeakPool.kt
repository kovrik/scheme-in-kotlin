package core.utils

import java.lang.ref.WeakReference
import java.util.WeakHashMap

internal class WeakPool<T> {

    private val pool = WeakHashMap<T, WeakReference<T>>()

    operator fun get(`object`: T): T? {
        val res: T?
        val ref = pool[`object`]
        when {
            ref != null -> res = ref.get()
            else -> res = null
        }
        return res
    }

    fun put(`object`: T) {
        pool.put(`object`, WeakReference(`object`))
    }
}
