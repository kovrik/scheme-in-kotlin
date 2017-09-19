package core.scm

class Repeat<T> constructor(private val value : T) : Sequence<T> {
    override fun iterator(): Iterator<T> = object : Iterator<T> {
        override fun next(): T = value
        override fun hasNext() = true
    }
}


