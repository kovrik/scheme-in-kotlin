package core.reader

interface IReader {

    /* Read input stream and parse it into a list of S-expressions. */
    fun read(): List<Any>
}
