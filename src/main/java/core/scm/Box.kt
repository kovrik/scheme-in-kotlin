package core.scm

import core.Writer
import java.util.concurrent.atomic.AtomicReference

class Box<T>(value: T) : AtomicReference<T>(value), IDeref {

    override fun deref(): T = get()

    override fun deref(timeout: Long, timeoutVal: Any?) = throw UnsupportedOperationException("deref with timeout is not available for boxes!")

    override fun toString() = get().let {
        when (it) {
            this -> "#<box!(this box)>"
            else -> "#<box!${Writer.write(it)}>"
        }
    }
}