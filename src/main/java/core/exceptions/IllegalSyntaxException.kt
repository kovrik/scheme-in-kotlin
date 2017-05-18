package core.exceptions

class IllegalSyntaxException(message: String) : RuntimeException(message) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }

    companion object {

        @JvmOverloads fun of(syntax: String, expression: Any, description: String? = null): IllegalSyntaxException {
            val message: String
            when {
                description != null -> message = String.format("%s: bad syntax (%s) in form: %s", syntax, description, expression)
                else -> message = String.format("%s: bad syntax in form: %s", syntax, expression)
            }
            return IllegalSyntaxException(message)
        }
    }
}
