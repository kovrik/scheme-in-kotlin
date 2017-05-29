package core.exceptions

import core.scm.Delay
import core.writer.Writer

class ReentrantDelayException(delay: Delay) : RuntimeException("re-entrant delay: ${Writer.write(delay)}", null) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
