package core.utils

class InternPool<T> {

    private val pool = WeakPool<T>()

    @Synchronized fun intern(`object`: T): T? {
        var res: T? = pool[`object`]
        if (res == null) {
            pool.put(`object`)
            res = `object`
        }
        return res
    }
}
