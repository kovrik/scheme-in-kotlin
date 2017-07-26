package core.reader

import java.io.IOException
import java.io.PushbackReader

class StringReader : Reader() {

    fun readOne(string: String): Any? {
        reader = PushbackReader(java.io.StringReader(string), 1)
        try {
            return nextToken()
        } catch (e: IOException) {
            e.printStackTrace()
        } finally {
            try {
                reader.close()
            } catch (ignore: IOException) {
            }
        }
        return null
    }

    fun read(string: String): List<Any>? {
        reader = PushbackReader(java.io.StringReader(string), 1)
        try {
            val tokens = ArrayList<Any>()
            var token = nextToken()
            while (token != null) {
                tokens.add(token)
                token = nextToken()
            }
            return tokens
        } catch (e: IOException) {
            e.printStackTrace()
        } finally {
            try {
                reader.close()
            } catch (ignore: IOException) {
            }
        }
        return null
    }
}
