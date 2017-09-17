package core.scm

import core.environment.Environment

data class LazySeq<T> @JvmOverloads constructor(val seq: Sequence<T>, val context: Environment? = null) : Sequence<T> by seq