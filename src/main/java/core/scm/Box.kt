package core.scm

import core.writer.Writer
import java.util.concurrent.atomic.AtomicReference

class Box<T>(value: T) : AtomicReference<T>(value), IDeref {

    override fun deref(): T = get()

    override fun deref(timeout: Long, timeoutVal: Any?): Any? = throw UnsupportedOperationException("deref with timeout is not available for boxes!")

    override fun toString() = "#<box!" + Writer.write(get()) + '>'
}