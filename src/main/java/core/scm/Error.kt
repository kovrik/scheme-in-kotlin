package core.scm

class Error(message: String) : RuntimeException(message) {
    override fun toString() = "Error: $message"
}
