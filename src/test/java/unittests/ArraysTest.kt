package unittests

import org.junit.Test

import kotlin.test.assertTrue

class ArraysTest : AbstractTest() {

    @Test
    fun testArraysAppend() {
        assertTrue(booleanArrayOf() contentEquals eval("(booleans-append (booleans) (booleans))") as BooleanArray)
        assertTrue(booleanArrayOf(true, false, true, false) contentEquals eval("(booleans-append (booleans #t #f) (booleans #t #f))") as BooleanArray)

        assertTrue(byteArrayOf() contentEquals eval("(bytes-append (bytes) (bytes))") as ByteArray)
        assertTrue(byteArrayOf(1, 2, 3, 4) contentEquals eval("(bytes-append (bytes 1 2) (bytes 3 4))") as ByteArray)

        assertTrue(charArrayOf() contentEquals eval("(chars-append (chars) (chars))") as CharArray)
        assertTrue(charArrayOf('a', 'b', 'c', 'd') contentEquals eval("(chars-append (chars #\\a #\\b) (chars #\\c #\\d))") as CharArray)

        assertTrue(doubleArrayOf() contentEquals eval("(doubles-append (doubles) (doubles))") as DoubleArray)
        assertTrue(doubleArrayOf(1.0, 2.0, 3.0, 4.0) contentEquals eval("(doubles-append (doubles 1 2) (doubles 3 4))") as DoubleArray)

        assertTrue(floatArrayOf() contentEquals eval("(floats-append (floats) (floats))") as FloatArray)
        assertTrue(floatArrayOf(1f, 2f, 3f, 4f) contentEquals eval("(floats-append (floats 1 2) (floats 3 4))") as FloatArray)

        assertTrue(intArrayOf() contentEquals eval("(ints-append (ints) (ints))") as IntArray)
        assertTrue(intArrayOf(1, 2, 3, 4) contentEquals eval("(ints-append (ints 1 2) (ints 3 4))") as IntArray)

        assertTrue(longArrayOf() contentEquals eval("(longs-append (longs) (longs))") as LongArray)
        assertTrue(longArrayOf(1L, 2L, 3L, 4L) contentEquals eval("(longs-append (longs 1 2) (longs 3 4))") as LongArray)

        assertTrue(shortArrayOf() contentEquals eval("(shorts-append (shorts) (shorts))") as ShortArray)
        assertTrue(shortArrayOf(1, 2, 3, 4) contentEquals eval("(shorts-append (shorts 1 2) (shorts 3 4))") as ShortArray)

        assertTrue(arrayOf<Any?>() contentEquals eval("(objects-append (objects) (objects))") as Array<Any?>)
        assertTrue(arrayOf<Any?>(1L, "a", 'b', emptyList<Nothing>()) contentEquals eval("(objects-append (objects 1 \"a\") (objects #\\b '()))") as Array<Any?>)
    }

    // TODO ArraysFill

    // TODO ArraysLength

    // TODO ArraysNew

    // TODO ArraysRef

    // TODO ArraysSet

    // TODO ArraysToList

    // TODO ListToArray

    // TODO MakeArrays

    // TODO SubArrays

    // TODO ToArray
}
