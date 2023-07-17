package unittests

import core.exceptions.WrongTypeException
import core.scm.MutableString
import core.scm.Symbol
import core.scm.Vector
import core.Writer
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

class StringTest : AbstractTest() {

    @Test
    fun testEvalIsString() {
        assertEquals(false, eval("(string? #\\A)"))
        assertEquals(true, eval("(string? \"A\")"))
    }

    @Test
    fun testEvalStrings() {
        assertEquals("1", eval("\"1\""))
        assertEquals("Lorem ipsum", eval("\"Lorem ipsum\""))
        assertEquals("Lorem \"ipsum\" ", eval("\"Lorem \\\"ipsum\\\" \""))
        assertEquals("", eval("\"\""))
        assertEquals(1, eval("(count \"\\\"\")"))
        assertEquals(5, eval("(count \"\t\b\n\r\\\"\")"))
    }

    @Test
    fun testEvalStringEq() {
        assertEquals(true, eval("(string=? \"test\" \"test\")"))
        assertEquals(false, eval("(string=? \"test\" \"test123\")"))
        assertEquals(true, eval("(string=? \"\" \"\")"))
        assertEquals(false, eval("(string=? \"test\" \"Test\")"))
    }

    @Test
    fun testEvalStringEqCi() {
        assertEquals(true, eval("(string-ci=? \"test\" \"test\")"))
        assertEquals(false, eval("(string-ci=? \"test\" \"test123\")"))
        assertEquals(true, eval("(string-ci=? \"\" \"\")"))
        assertEquals(true, eval("(string-ci=? \"test\" \"Test\")"))
        assertEquals(true, eval("(string-ci=? \"tESt\" \"TesT\")"))
    }

    @Test
    fun testEvalStringProc() {
        assertEquals(MutableString(""), eval("(string)"))
        assertEquals(MutableString("a"), eval("(string #\\a)"))
        assertEquals(MutableString("abc"), eval("(string #\\a #\\b #\\c)"))

        try {
            eval("(string 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalMakeString() {
        assertEquals(MutableString(""), eval("(make-string 0)"))
        assertEquals(MutableString(""), eval("(make-string 0 #\\a)"))
        assertEquals(MutableString("a"), eval("(make-string 1 #\\a)"))
        assertEquals(MutableString("aa"), eval("(make-string 2 #\\a)"))
        assertEquals(MutableString("ZZZZZZZZ"), eval("(make-string 8 #\\Z)"))
        assertEquals(MutableString("\u0000\u0000\u0000"), eval("(make-string 3)"))
        assertEquals(MutableString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"), eval("(make-string 8)"))

        try {
            eval("(make-string \"test\")")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: type mismatch; (expected: ExactNonNegativeInteger, given: \"test\")", e.message)
        }

        try {
            eval("(make-string 2 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: type mismatch; (expected: Character, given: 1)", e.message)
        }

        try {
            eval("(make-string)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 0)", e.message)
        }

        try {
            eval("(make-string 1 2 3)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 3)", e.message)
        }
    }

    @Test
    fun testEvalStringFill() {
        try {
            eval("(string-fill! \"\" #\\a)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"\")", e.message)
        }

        try {
            eval("(string-fill! \"z\" #\\a)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"z\")", e.message)
        }

        try {
            eval("(string-fill! \"test1\" #\\a)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"test1\")", e.message)
        }

        assertEquals(MutableString(""), eval("(string-fill! (make-string 0) #\\a)"))
        assertEquals(MutableString("a"), eval("(string-fill! (make-string 1 #\\z) #\\a)"))
        assertEquals(MutableString("aaaaa"), eval("(string-fill! (string #\\t #\\e #\\s #\\t #\\u0001) #\\a)"))
    }

    @Test
    fun testEvalStringCopy() {
        assertEquals(MutableString(""), eval("(string-copy \"\")"))
        assertEquals(MutableString("test"), eval("(string-copy \"test\")"))
        assertEquals(MutableString("t"), eval("(string-copy \"t\")"))
    }

    @Test
    fun testEvalStringAppend() {
        assertEquals("", eval("(string-append)"))
        assertEquals("", eval("(string-append \"\")"))
        assertEquals("Apple", eval("(string-append \"Apple\")"))
        assertEquals("AppleBanana", eval("(string-append \"Apple\" \"Banana\")"))
        assertEquals("AppleBananaCoconut", eval("(string-append \"Apple\" \"Banana\" \"Coconut\")"))
    }

    @Test
    fun testEvalStringLength() {
        assertEquals(0L, eval("(string-length \"\")"))
        assertEquals(0L, eval("(string-length (string))"))
        assertEquals(1L, eval("(string-length \"1\")"))
        assertEquals(3L, eval("(string-length \"123\")"))

        try {
            eval("(string-length 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-length: type mismatch; (expected: String, given: 1)", e.message)
        }
    }

    @Test
    fun testStringToList() {
        assertEquals(listOf('a', 'b', 'c'), eval("(string->list \"abc\")"))
        assertEquals(listOf('a'), eval("(string->list \"a\")"))
        assertEquals(listOf<Any>(), eval("(string->list \"\")"))
        try {
            eval("(string->list (cons 1 2))")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string->list: type mismatch; (expected: String, given: (1 . 2))", e.message)
        }
    }

    @Test
    fun testEvalStringRef() {
        assertEquals('t', eval("(string-ref \"test string\" 0)"))
        assertEquals('e', eval("(string-ref \"test string\" 1)"))
        assertEquals('s', eval("(string-ref \"test string\" 2)"))

        try {
            eval("(string-ref \"test\" -1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }

        try {
            eval("(string-ref \"tes\" 3)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-ref: value out of range: 3", e.message)
        }

        try {
            eval("(string-ref \"\" 0)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-ref: value out of range: 0", e.message)
        }

        try {
            eval("(string-ref '(1 2 3) 0)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: String, given: (1 2 3))", e.message)
        }

        try {
            eval("(string-ref \"test\" 0.5)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }

    }

    @Test
    fun testEvalStringSet() {
        assertEquals(MutableString("z"), eval("(let ((s (string #\\a) )) (string-set! s 0 #\\z) s)"))
        assertEquals(MutableString("zbc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 0 #\\z) s)"))
        assertEquals(MutableString("azc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 1 #\\z) s)"))
        assertEquals(MutableString("abz"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 2 #\\z) s)"))

        try {
            eval("(let ((s \"a\"  )) (string-set! s 0 #\\z) s)")
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-set!: type mismatch; (expected: Mutable String, given: \"a\")", e.message)
        }

        try {
            eval("(string-set! (string #\\a #\\b #\\c) -1 #\\z)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }

        try {
            eval("(string-set! (string #\\a #\\b #\\c) 3 #\\z)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-set!: value out of range: 3", e.message)
        }

        try {
            eval("(string-set! (make-string 0) 0 #\\z)")
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-set!: value out of range: 0", e.message)
        }

        try {
            eval("(string-set! '(1 2 3) 2 #\\z)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: Mutable String, given: (1 2 3))", e.message)
        }

        try {
            eval("(string-set! (make-string 4) 0.5 #\\A)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }

        try {
            eval("(string-set! (make-string 4) 3 '())")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: Character, given: ())", e.message)
        }
    }

    @Test
    fun testEvalSymbolStringConversion() {
        assertEquals("test", eval("(symbol->string 'test)"))
        assertEquals("test", eval("(symbol->string (string->symbol (symbol->string 'test)))"))
        assertEquals("TeSt", eval("(symbol->string (string->symbol (symbol->string 'TeSt)))"))
        assertEquals("", eval("(symbol->string (string->symbol \"\"))"))
        assertEquals("123", eval("(symbol->string (string->symbol \"123\"))"))
        assertEquals("6", eval("(symbol->string (string->symbol \"6\"))"))
        assertEquals("6bsdf", eval("(symbol->string (string->symbol \"6bsdf\"))"))
        assertEquals("one two three", eval("(symbol->string (string->symbol \"one two three\"))"))
        assertEquals("  ", eval("(symbol->string (string->symbol \"  \"))"))
        assertEquals("||", Writer.write(eval("(string->symbol \"\")")))
        assertEquals("|123|", Writer.write(eval("(string->symbol \"123\")")))
        assertEquals("|6|", Writer.write(eval("(string->symbol \"6\")")))
        assertEquals("|6bsdf|", Writer.write(eval("(string->symbol \"6bsdf\")")))
        assertEquals("|one two three|", Writer.write(eval("(string->symbol \"one two three\")")))
        assertEquals("|  |", Writer.write(eval("(string->symbol \"  \")")))
        //    assertEquals("|.|", Writer.write(eval("(string->symbol \".\")")));
        assertEquals("test.", Writer.write(eval("(string->symbol \"test.\")")))
        assertEquals("|#|", Writer.write(eval("(string->symbol \"#\")")))
        assertEquals("|#123|", Writer.write(eval("(string->symbol \"#123\")")))
        assertEquals("|#abc|", Writer.write(eval("(string->symbol \"#abc\")")))
        assertEquals("a#bc", Writer.write(eval("(string->symbol \"a#bc\")")))
        assertEquals("abc#", Writer.write(eval("(string->symbol \"abc#\")")))
        assertEquals("#%abc", Writer.write(eval("(string->symbol \"#%abc\")")))
        assertEquals("(|a b c|)", Writer.write(eval("(list (string->symbol \"a b c\"))")))
        assertEquals(Symbol.intern("test"), eval("(string->symbol (symbol->string 'test))"))
        try {
            eval("(symbol->string 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("symbol->string: type mismatch; (expected: Symbol, given: 1)", e.message)
        }

        try {
            eval("(string->symbol 1)")
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string->symbol: type mismatch; (expected: String, given: 1)", e.message)
        }
    }

    @Test
    fun testStringInterning() {
        // immutable interned strings must have the same hashcode
        assertEquals(true, eval("(apply = (map hashcode '(\"test\" \"test\" \"test\" \"test\" \"test\")))"))

        // immutable interned strings must point to the same object
        assertEquals(true, eval("(eq? \"test\" \"test\" \"test\" \"test\" \"test\")"))
        assertEquals(true, eval("(eqv? \"test\" \"test\" \"test\" \"test\" \"test\")"))
        assertEquals(true, eval("(equal? \"test\" \"test\" \"test\" \"test\" \"test\")"))
        // mutable interned strings
        assertEquals(false, eval("(eq? (string #\\a) (string #\\a))"))
        assertEquals(false, eval("(eqv? (string #\\a) (string #\\a))"))
        assertEquals(true, eval("(equal? (string #\\a) (string #\\a))"))
        assertEquals(true, eval("(equal? \"a\" (string #\\a))"))
        assertEquals(true, eval("(equal? (string #\\a) \"a\" )"))
        assertEquals(true, eval("(mutable? (string #\\a))"))
        assertEquals(false, eval("(immutable? (string #\\a))"))
        assertEquals(true, eval("(immutable? \"a\")"))
        assertEquals(false, eval("(mutable? \"a\")"))
        assertEquals(true, eval("(immutable? (string->immutable-string (string #\\a)))"))
    }

    @Test
    fun testIsBlank() {
        assertEquals(true, eval("(blank? nil)"))
        assertEquals(true, eval("(blank? \"\")"))
        assertEquals(true, eval("(blank? \"      \")"))
        assertEquals(true, eval("(blank? \" \t\t\r\n    \")"))
        assertEquals(false, eval("(blank? \" \t\ta\r\n    \")"))
        assertEquals(false, eval("(blank? \"not-blank\")"))
    }

    @Test
    fun testSplit() {
        assertEquals(3, (eval("(split \"a,b,c,\" #\",\")") as Vector).size.toLong())
        assertEquals(1, (eval("(split \"a,b,c,\" #\"_\")") as Vector).size.toLong())
        assertEquals(4, (eval("(split \"a,b,c,\" #\",\" -1)") as Vector).size.toLong())
        assertEquals(10, (eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\")") as Vector).size.toLong())
        assertEquals(5, (eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\" 5)") as Vector).size.toLong())
    }

    @Test
    fun testJoin() {
        assertEquals("", eval("(join \"\")"))
        assertEquals(",", eval("(join \",\")"))
        assertEquals("", eval("(join \",\" [])"))
        assertEquals("", eval("(join \",\" '())"))
        assertEquals("", eval("(join \",\" \"\")"))
        assertEquals("test", eval("(join \"\" [#\\t #\\e #\\s #\\t])"))
        assertEquals("test", eval("(join \"\" '(#\\t #\\e #\\s #\\t))"))
        assertEquals("test", eval("(join \"\" \"test\")"))
        assertEquals("t,e,s,t", eval("(join \",\" [#\\t #\\e #\\s #\\t])"))
        assertEquals("t,e,s,t", eval("(join \",\" '(#\\t #\\e #\\s #\\t))"))
        assertEquals("t,e,s,t", eval("(join \",\" \"test\")"))
        assertEquals("t123e123s123t", eval("(join \"123\" [#\\t #\\e #\\s #\\t])"))
        assertEquals("t123e123s123t", eval("(join \"123\" '(#\\t #\\e #\\s #\\t))"))
        assertEquals("t123e123s123t", eval("(join \"123\" \"test\")"))
    }

    @Test
    fun testReplace() {
        assertEquals("fabulous ddero oo doo", eval("(replace \"fabulous fodder foo food\" #\"f(o+)(\\S+)\" \"$2$1\")"))
        assertEquals("The color is blue", eval("(replace \"The color is red\" #\"red\" \"blue\")"))
        assertEquals("Vegeta", eval("(replace \"Vegeta\" #\"Goku\" \"Gohan\")"))
    }

    @Test
    fun testReplaceFirst() {
        assertEquals("A good night to you, sir.  Good day.", eval("(replace-first \"A good day to you, sir.  Good day.\" #\"day\" \"night\")"))
        assertEquals("A good day to you, sir.", eval("(replace-first \"A good day to you, sir.\" #\"madam\" \"master\")"))
        assertEquals("night need not be SHOUTED.", eval("(replace-first \"Day need not be SHOUTED.\" #\"(?i)day\" \"night\")"))
        assertEquals("name", eval("(replace-first \"/path/to/file/name\" #\"^.*/\" \"\")"))
        assertEquals("path/to/file/name", eval("(replace-first \"/path/to/file/name\" #\"^.*?/\" \"\")"))
    }
}
