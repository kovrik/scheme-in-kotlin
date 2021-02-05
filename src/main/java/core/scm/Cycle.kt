package core.scm

class Cycle<T> constructor(private val sequence : Sequence<T>) : Sequence<T> {

    override fun iterator(): Iterator<T> = object : Iterator<T> {

        @Volatile private var iterator = sequence.iterator()

        override fun next() = iterator.next().also { if (!iterator.hasNext()) { iterator = sequence.iterator() } }

        override fun hasNext() = iterator.hasNext()
    }
}

/* Create cycled version of sequence */
fun Sequence<*>.cycled() = Cycle(this)

