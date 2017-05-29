package core.exceptions

class IllegalSyntaxException(message: String) : RuntimeException(message) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }

    companion object {

        @JvmOverloads fun of(syntax: String, expression: Any, description: String? = null): IllegalSyntaxException {
            val message: String
            when (description) {
                null -> message = "$syntax: bad syntax in form: $expression"
                else -> message = "$syntax: bad syntax ($description) in form: $expression"
            }
            return IllegalSyntaxException(message)
        }
    }
}
