package core.reader

import core.exceptions.ThrowableWrapper
import java.io.*

class FileReader : Reader() {

    fun read(file: File): List<Any> {
        try {
            reader = PushbackReader(BufferedReader(java.io.FileReader(file)), 1)
        } catch (e: FileNotFoundException) {
            throw ThrowableWrapper(e)
        }
        val tokens = ArrayList<Any>()
        try {
            var i = reader.read()
            while (i != -1) {
                reader.unread(i)
                nextToken()?.let { tokens.add(it) }
                i = reader.read()
            }
        } catch (e: IOException) {
            throw ThrowableWrapper(e)
        } finally {
            try {
                reader.close()
            } catch (ignore: IOException) {
            }
        }
        return tokens
    }
}
