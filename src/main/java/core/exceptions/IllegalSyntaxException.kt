package core.exceptions

class IllegalSyntaxException(message: String) : RuntimeException(message) {

    @Synchronized override fun fillInStackTrace() = null

    companion object {

        @JvmOverloads fun of(syntax: String, expression: Any, description: String? = null): IllegalSyntaxException {
            val message = when (description) {
                null -> "$syntax: bad syntax in form: $expression"
                else -> "$syntax: bad syntax ($description) in form: $expression"
            }
            return IllegalSyntaxException(message)
        }
    }
}
