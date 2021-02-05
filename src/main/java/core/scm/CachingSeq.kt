package core.scm

/* Sequence wrapper that caches results */
class CachingSeq<out T> private constructor(private val source: Iterator<T>,
                                            private val cache: MutableList<T>,
                                            private val start: Int) : Sequence<T> {

    constructor(source: Sequence<T>) : this(source.iterator(), mutableListOf(), 0)

    override fun iterator() = object : Iterator<T> {

        private var pos = start

        override fun hasNext() = pos < cache.size || source.hasNext()

        override fun next(): T {
            while (pos >= cache.size) { cache.add(source.next()) }
            return cache[pos++]
        }
    }
}

/* Create caching version of sequence */
fun Sequence<*>.cached() = CachingSeq(this)
