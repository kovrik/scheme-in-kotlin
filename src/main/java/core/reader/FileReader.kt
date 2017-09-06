package core.reader

import java.io.*

class FileReader : Reader() {

    fun read(file: File): List<Any> = run {
        reader = PushbackReader(BufferedReader(java.io.FileReader(file)), 1)
        reader.use {
            ArrayList<Any>().apply {
                var i = reader.read()
                while (i != -1) {
                    reader.unread(i)
                    nextToken()?.let { add(it) }
                    i = reader.read()
                }
            }
        }
    }
}
