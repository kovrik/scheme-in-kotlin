package core.exceptions

class UndefinedIdentifierException(identifier: String) : RuntimeException("unable to resolve symbol: " + identifier) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
