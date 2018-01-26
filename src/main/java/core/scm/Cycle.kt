package core.scm

class Cycle<T> constructor(private val sequence : Sequence<T>) : Sequence<T> {

    override fun iterator(): Iterator<T> = object : Iterator<T> {

        @Volatile private var iterator = sequence.iterator()

        override fun next(): T {
            val result = iterator.next()
            if (!iterator.hasNext()) {
                iterator = sequence.iterator()
            }
            return result
        }

        override fun hasNext() = iterator.hasNext()
    }
}

