package core.exceptions

import core.scm.Delay
import core.Writer

class ReentrantDelayException(delay: Delay) : RuntimeException("re-entrant delay: ${Writer.write(delay)}", null) {

    @Synchronized override fun fillInStackTrace() = null
}
