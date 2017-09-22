package core.exceptions

class IllegalSyntaxException(message: String) : RuntimeException(message) {

    @Synchronized override fun fillInStackTrace() = null

    constructor(syntax: String, expression: String, description: String? = null) : this(when (description) {
        null -> "$syntax: bad syntax in form: $expression"
        else -> "$syntax: bad syntax ($description) in form: $expression"
    })
}