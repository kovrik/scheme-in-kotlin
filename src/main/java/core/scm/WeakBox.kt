package core.scm

import core.Writer
import java.lang.ref.WeakReference

class WeakBox<T>(value: T) : WeakReference<T>(value), IDeref {

    override fun deref(): T? = get()

    override fun deref(timeout: Long, timeoutVal: Any?) = throw UnsupportedOperationException("deref with timeout is not available for boxes!")

    override fun toString() = "#<weak-box!" + Writer.write(get()) + '>'
}