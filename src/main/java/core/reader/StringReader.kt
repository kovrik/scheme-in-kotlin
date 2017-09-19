package core.reader

import java.io.PushbackReader

class StringReader : Reader() {

    fun readOne(string: String) = PushbackReader(java.io.StringReader(string), 1).use {
        reader = it
        nextToken()
    }

    fun read(string: String): List<Any?> = PushbackReader(java.io.StringReader(string), 1).use {
        reader = it
        mutableListOf<Any?>().apply {
            var token = nextToken()
            while (token != Unit) {
                add(token)
                token = nextToken()
            }
        }
    }
}
