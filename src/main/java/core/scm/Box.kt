package core.scm

import core.writer.Writer
import java.util.concurrent.atomic.AtomicReference

class Box<T>(value: T) : AtomicReference<T>(value), IDeref {

    override fun deref(): T = get()

    override fun toString() = "#<box!" + Writer.write(get()) + '>'
}