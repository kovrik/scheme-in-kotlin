package core.exceptions

import core.scm.Delay
import core.writer.Writer

class ReentrantDelayException(delay: Delay) : RuntimeException(String.format("Re-entrant delay: %s", Writer.write(delay)), null) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
