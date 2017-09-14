package unittests

import core.exceptions.ArityException
import core.scm.Cons
import core.scm.Keyword
import core.scm.MapEntry
import core.scm.MutableVector
import org.junit.Assert.*
import org.junit.Test

class HashmapTest : AbstractTest() {

    @Test
    fun testHashmap() {
        assertTrue(eval("{}", env) is Map<*, *>)
        assertTrue(eval("{1 2}", env) is Map<*, *>)
        assertTrue(eval("{1 2, 3 4}", env) is Map<*, *>)
        assertTrue(eval("{1,2,3,4}", env) is Map<*, *>)
        assertTrue(eval("{1,2,,3,4}", env) is Map<*, *>)
        assertTrue(eval("{1 2, 3 4 ,   5   8 }", env) is Map<*, *>)
        assertTrue(eval("(hash-map 1 2 3 4    5   8 )", env) is Map<*, *>)
    }

    @Test
    fun testHashmapGet() {
        assertEquals(5L, eval("({3 5} 3)", env))
        assertEquals(5L, eval("({3 5} 7 5)", env))
        assertEquals(5L, eval("(get {3 5} 3)", env))
        assertEquals(5L, eval("(get {3 5} 7 5)", env))
        assertEquals("B", eval("({3 5, \"A\" \"B\"} \"A\" 5)", env))
        assertEquals("B", eval("((hash-map 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals("B", eval("(get {3 5, \"A\" \"B\"} \"A\" 5)", env))
        assertEquals("B", eval("(get (hash-map 3 5 \"A\" \"B\") \"A\" 5)", env))

        assertEquals(5L, eval("(.get {3 5} 3)", env))
        assertEquals(5L, eval("(.getOrDefault {3 5} 7 5)", env))
        assertEquals("B", eval("(.getOrDefault {3 5, \"A\" \"B\"} \"A\" 5)", env))
        assertEquals("B", eval("(.getOrDefault (hash-map 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals(5L, eval("({'a 5} 'a)", env))
        assertEquals(4L, eval("({'a 5 'b 3 'c 4} 'c)", env))
        assertEquals(5L, eval("('a {'a 5})", env))
        assertEquals(4L, eval("('c {'a 5 'b 3 'c 4})", env))
        assertEquals(5L, eval("({:a 5} :a)", env))
        assertEquals(4L, eval("({:a 5 :b 3 :c 4} :c)", env))
        assertEquals(5L, eval("(:a {:a 5})", env))
        assertEquals(4L, eval("(:c {:a 5 :b 3 :c 4})", env))
    }

    @Test
    fun testHashmapPut() {
        assertEquals(5L, eval("((put {} 3 5) 3)", env))
        assertEquals(5L, eval("((put {} 3 (+ 2 3)) 7 5)", env))
        assertEquals(5L, eval("(get (put {} 3 (+ 2 3)) (+ 1 2))", env))
        assertEquals(5L, eval("(get (put {} 3 (+ 2 3)) 7 5)", env))
        assertEquals("B", eval("((put {} 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals("B", eval("((put (hash-map) 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals("B", eval("(get (put {} 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals("B", eval("(get (put (hash-map) 3 5 \"A\" \"B\") \"A\" 5)", env))
        assertEquals(MutableVector(arrayOf(Keyword.intern("a"), 2L, 3L)), eval("(put [1 2 3] 0 :a)", env))
        assertEquals(MutableVector(arrayOf(1L, Keyword.intern("a"), 3L)), eval("(put [1 2 3] 1 :a)", env))
        assertEquals(MapEntry(Keyword.intern("c"), 1L), eval("(put (first {:a 1}) 0 :c)", env))
        assertEquals(MapEntry(Keyword.intern("a"), Keyword.intern("c")), eval("(put (first {:a 1}) 1 :c)", env))
    }

    @Test
    fun testHashmapEval() {
        assertEquals(8L,  eval("({(+ 1 2 3) (* 2 4)} 6)", env))
        assertEquals(8L,  eval("((hash-map (+ 1 2 3) (* 2 4)) 6)", env))
        assertEquals(10L, eval("(({* *, + +} +) 1 2 3 4)", env))
        assertEquals(10L, eval("(((hash-map * * + +) +) 1 2 3 4)", env))
        try {
            eval("({:test true} 1 2 3)", env)
            fail()
        } catch (e: ArityException) {
            // success
        }
    }

    @Test
    fun testHashmapKeysValues() {
        assertEquals(Cons.list(1L, 2L, 3L), eval("(keys {1 + 2 - 3 /})", env))
        assertEquals(Cons.EMPTY, eval("(keys {})", env))
        assertEquals(6L, eval("(apply + (vals (zipmap '[+ - /] '(1 2 3))))", env))
        assertEquals(Cons.EMPTY, eval("(vals {})", env))
        assertEquals(emptyMap<Any?, Any?>(), eval("(zipmap [] '())", env))
        assertEquals(emptyMap<Any?, Any?>(), eval("(zipmap nil nil)", env))
        assertEquals(3L, eval("(get (zipmap \"test\" [1 2 3 4]) #\\s)", env))
    }

    @Test
    fun testHashmapEntries() {
        assertEquals(2, eval("(count  (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(1L, eval("(second (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(Keyword.intern("a"), eval("(key   (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(Keyword.intern("a"), eval("(first (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(Keyword.intern("a"), eval("(get   (first {:a 1 :b 2 :c 3}) 0)", env))
        assertEquals(Keyword.intern("a"), eval("(nth   (first {:a 1 :b 2 :c 3}) 0)", env))
        assertEquals(1L, eval("(val (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(1L, eval("(get (first {:a 1 :b 2 :c 3}) 1)", env))
        assertEquals(1L, eval("(nth (first {:a 1 :b 2 :c 3}) 1)", env))
        assertEquals(listOf(1L, Keyword.intern("a")), eval("(reverse (first {:a 1 :b 2 :c 3}))", env))
        assertEquals(listOf(Keyword.intern("a"), 1L), eval("(reverse (reverse (first {:a 1 :b 2 :c 3})))", env))
    }

    @Test
    fun testFind() {
        assertEquals(null, eval("(find {:a 1 :b 2 :c 3} :d)", env))
        assertEquals(MapEntry(Keyword.intern("a"), 1L), eval("(find {:a 1 :b 2 :c 3} :a)", env))
        assertEquals(MapEntry(0, Keyword.intern("a")), eval("(find (first {:a 1 :b 2 :c 3}) 0)", env))
        assertEquals(MapEntry(1, 1L), eval("(find (first {:a 1 :b 2 :c 3}) 1)", env))
        assertEquals(MapEntry(0, Keyword.intern("a")), eval("(find [:a :b :c :d] 0)", env))
        assertEquals(MapEntry(2, Keyword.intern("c")), eval("(find [:a :b :c :d] 2)", env))
        assertEquals(null, eval("(find [:a :b :c :d] 5)", env))
        assertEquals(null, eval("(find nil 5)", env))
    }

    @Test
    fun testHashmapNext() {
        assertEquals(null, eval("(next {})", env))
        assertEquals(null, eval("(next {:a 1})", env))
        assertEquals(1,    eval("(count (next {:a 1 :b 2}))", env))
    }

    @Test
    fun testHashmapRest() {
        assertEquals(emptyList<Any?>(), (eval("(rest {})", env) as Sequence<*>).toList())
        assertEquals(emptyList<Any?>(), (eval("(rest {:a 1})", env) as Sequence<*>).toList())
        assertEquals(1, eval("(count (rest {:a 1 :b 2}))", env))
    }
}
