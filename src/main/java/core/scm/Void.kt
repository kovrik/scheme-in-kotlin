package core.scm

/**
 * Void singleton
 * Nil result (null) is a valid result,
 * so we need this VOID class to represent actual void result
 * (same as #<unspecified> in Scheme) */
object Void {
    override fun toString() = "#<void>"
}
