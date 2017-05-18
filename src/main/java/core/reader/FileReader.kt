package core.reader

import core.exceptions.SCMFileNotFoundException
import java.io.*
import java.util.*

class FileReader : Reader() {

    fun read(file: File): List<Any> {
        try {
            reader = PushbackReader(BufferedReader(java.io.FileReader(file)), 1)
        } catch (e: FileNotFoundException) {
            throw SCMFileNotFoundException(file.path)
        }

        val tokens = ArrayList<Any>()
        try {
            var i = reader.read()
            while (i != -1) {
                reader.unread(i)
                val token = nextToken()
                if (token != null) {
                    tokens.add(token)
                }
                i = reader.read()
            }
        } catch (e: IOException) {
            e.printStackTrace()
        } finally {
            try {
                reader.close()
            } catch (ignore: IOException) {
            }
        }
        return tokens
    }
}
