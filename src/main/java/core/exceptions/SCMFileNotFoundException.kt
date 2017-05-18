package core.exceptions

class SCMFileNotFoundException(filename: String) : RuntimeException("Cannot open file: " + filename) {

    @Synchronized override fun fillInStackTrace(): Throwable? {
        return null
    }
}
