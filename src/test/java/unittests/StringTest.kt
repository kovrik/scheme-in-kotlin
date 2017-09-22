package unittests

import core.exceptions.WrongTypeException
import core.scm.MutableString
import core.scm.Symbol
import core.scm.Vector
import core.writer.Writer
import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

class StringTest : AbstractTest() {

    @Test
    fun testEvalIsString() {
        assertEquals(false, eval("(string? #\\A)", env))
        assertEquals(true, eval("(string? \"A\")", env))
    }

    @Test
    fun testEvalStrings() {
        assertEquals("1", eval("\"1\"", env))
        assertEquals("Lorem ipsum", eval("\"Lorem ipsum\"", env))
        assertEquals("Lorem \"ipsum\" ", eval("\"Lorem \\\"ipsum\\\" \"", env))
        assertEquals("", eval("\"\"", env))
        assertEquals(1, eval("(count \"\\\"\")", env))
        assertEquals(5, eval("(count \"\t\b\n\r\\\"\")", env))
    }

    @Test
    fun testEvalStringEq() {
        assertEquals(true, eval("(string=? \"test\" \"test\")", env))
        assertEquals(false, eval("(string=? \"test\" \"test123\")", env))
        assertEquals(true, eval("(string=? \"\" \"\")", env))
        assertEquals(false, eval("(string=? \"test\" \"Test\")", env))
    }

    @Test
    fun testEvalStringEqCi() {
        assertEquals(true, eval("(string-ci=? \"test\" \"test\")", env))
        assertEquals(false, eval("(string-ci=? \"test\" \"test123\")", env))
        assertEquals(true, eval("(string-ci=? \"\" \"\")", env))
        assertEquals(true, eval("(string-ci=? \"test\" \"Test\")", env))
        assertEquals(true, eval("(string-ci=? \"tESt\" \"TesT\")", env))
    }

    @Test
    fun testEvalStringProc() {
        assertEquals(MutableString(""), eval("(string)", env))
        assertEquals(MutableString("a"), eval("(string #\\a)", env))
        assertEquals(MutableString("abc"), eval("(string #\\a #\\b #\\c)", env))

        try {
            eval("(string 1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string: type mismatch; (expected: Character, given: 1)", e.message)
        }
    }

    @Test
    fun testEvalMakeString() {
        assertEquals(MutableString(""), eval("(make-string 0)", env))
        assertEquals(MutableString(""), eval("(make-string 0 #\\a)", env))
        assertEquals(MutableString("a"), eval("(make-string 1 #\\a)", env))
        assertEquals(MutableString("aa"), eval("(make-string 2 #\\a)", env))
        assertEquals(MutableString("ZZZZZZZZ"), eval("(make-string 8 #\\Z)", env))
        assertEquals(MutableString("\u0000\u0000\u0000"), eval("(make-string 3)", env))
        assertEquals(MutableString("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"), eval("(make-string 8)", env))

        try {
            eval("(make-string \"test\")", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: type mismatch; (expected: ExactNonNegativeInteger, given: \"test\")", e.message)
        }

        try {
            eval("(make-string 2 1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: type mismatch; (expected: Character, given: 1)", e.message)
        }

        try {
            eval("(make-string)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 0)", e.message)
        }

        try {
            eval("(make-string 1 2 3)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("make-string: arity mismatch; the expected number of arguments does not match the given number (expected: 1 to 2, given: 3)", e.message)
        }
    }

    @Test
    fun testEvalStringFill() {
        try {
            eval("(string-fill! \"\" #\\a)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"\")", e.message)
        }

        try {
            eval("(string-fill! \"z\" #\\a)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"z\")", e.message)
        }

        try {
            eval("(string-fill! \"test1\" #\\a)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-fill!: type mismatch; (expected: Mutable String, given: \"test1\")", e.message)
        }

        assertEquals(MutableString(""), eval("(string-fill! (make-string 0) #\\a)", env))
        assertEquals(MutableString("a"), eval("(string-fill! (make-string 1 #\\z) #\\a)", env))
        assertEquals(MutableString("aaaaa"), eval("(string-fill! (string #\\t #\\e #\\s #\\t #\\u0001) #\\a)", env))
    }

    @Test
    fun testEvalStringCopy() {
        assertEquals(MutableString(""), eval("(string-copy \"\")", env))
        assertEquals(MutableString("test"), eval("(string-copy \"test\")", env))
        assertEquals(MutableString("t"), eval("(string-copy \"t\")", env))
    }

    @Test
    fun testEvalStringAppend() {
        assertEquals("", eval("(string-append)", env))
        assertEquals("", eval("(string-append \"\")", env))
        assertEquals("Apple", eval("(string-append \"Apple\")", env))
        assertEquals("AppleBanana", eval("(string-append \"Apple\" \"Banana\")", env))
        assertEquals("AppleBananaCoconut", eval("(string-append \"Apple\" \"Banana\" \"Coconut\")", env))
    }

    @Test
    fun testEvalStringLength() {
        assertEquals(0L, eval("(string-length \"\")", env))
        assertEquals(0L, eval("(string-length (string))", env))
        assertEquals(1L, eval("(string-length \"1\")", env))
        assertEquals(3L, eval("(string-length \"123\")", env))

        try {
            eval("(string-length 1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-length: type mismatch; (expected: String, given: 1)", e.message)
        }
    }

    @Test
    fun testStringToList() {
        assertEquals(listOf('a', 'b', 'c'), eval("(string->list \"abc\")", env))
        assertEquals(listOf('a'), eval("(string->list \"a\")", env))
        assertEquals(listOf<Any>(), eval("(string->list \"\")", env))
        try {
            eval("(string->list (cons 1 2))", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string->list: type mismatch; (expected: String, given: (1 . 2))", e.message)
        }
    }

    @Test
    fun testEvalStringRef() {
        assertEquals('t', eval("(string-ref \"test string\" 0)", env))
        assertEquals('e', eval("(string-ref \"test string\" 1)", env))
        assertEquals('s', eval("(string-ref \"test string\" 2)", env))

        try {
            eval("(string-ref \"test\" -1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }

        try {
            eval("(string-ref \"tes\" 3)", env)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-ref: value out of range: 3", e.message)
        }

        try {
            eval("(string-ref \"\" 0)", env)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-ref: value out of range: 0", e.message)
        }

        try {
            eval("(string-ref '(1 2 3) 0)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: String, given: (1 2 3))", e.message)
        }

        try {
            eval("(string-ref \"test\" 0.5)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-ref: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }

    }

    @Test
    fun testEvalStringSet() {
        assertEquals(MutableString("z"), eval("(let ((s (string #\\a) )) (string-set! s 0 #\\z) s)", env))
        assertEquals(MutableString("zbc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 0 #\\z) s)", env))
        assertEquals(MutableString("azc"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 1 #\\z) s)", env))
        assertEquals(MutableString("abz"), eval("(let ((s (string #\\a #\\b #\\c))) (string-set! s 2 #\\z) s)", env))

        try {
            eval("(let ((s \"a\"  )) (string-set! s 0 #\\z) s)", env)
            fail()
        } catch (e: WrongTypeException) {
            assertEquals("string-set!: type mismatch; (expected: Mutable String, given: \"a\")", e.message)
        }

        try {
            eval("(string-set! (string #\\a #\\b #\\c) -1 #\\z)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: -1)", e.message)
        }

        try {
            eval("(string-set! (string #\\a #\\b #\\c) 3 #\\z)", env)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-set!: value out of range: 3", e.message)
        }

        try {
            eval("(string-set! (make-string 0) 0 #\\z)", env)
            fail()
        } catch (e: IndexOutOfBoundsException) {
            assertEquals("string-set!: value out of range: 0", e.message)
        }

        try {
            eval("(string-set! '(1 2 3) 2 #\\z)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: Mutable String, given: (1 2 3))", e.message)
        }

        try {
            eval("(string-set! (make-string 4) 0.5 #\\A)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: ExactNonNegativeInteger, given: 0.5)", e.message)
        }

        try {
            eval("(string-set! (make-string 4) 3 '())", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string-set!: type mismatch; (expected: Character, given: ())", e.message)
        }
    }

    @Test
    fun testEvalSymbolStringConversion() {
        assertEquals("test", eval("(symbol->string 'test)", env))
        assertEquals("test", eval("(symbol->string (string->symbol (symbol->string 'test)))", env))
        assertEquals("TeSt", eval("(symbol->string (string->symbol (symbol->string 'TeSt)))", env))
        assertEquals("", eval("(symbol->string (string->symbol \"\"))", env))
        assertEquals("123", eval("(symbol->string (string->symbol \"123\"))", env))
        assertEquals("6", eval("(symbol->string (string->symbol \"6\"))", env))
        assertEquals("6bsdf", eval("(symbol->string (string->symbol \"6bsdf\"))", env))
        assertEquals("one two three", eval("(symbol->string (string->symbol \"one two three\"))", env))
        assertEquals("  ", eval("(symbol->string (string->symbol \"  \"))", env))
        assertEquals("||", Writer.write(eval("(string->symbol \"\")", env)))
        assertEquals("|123|", Writer.write(eval("(string->symbol \"123\")", env)))
        assertEquals("|6|", Writer.write(eval("(string->symbol \"6\")", env)))
        assertEquals("|6bsdf|", Writer.write(eval("(string->symbol \"6bsdf\")", env)))
        assertEquals("|one two three|", Writer.write(eval("(string->symbol \"one two three\")", env)))
        assertEquals("|  |", Writer.write(eval("(string->symbol \"  \")", env)))
        //    assertEquals("|.|", Writer.write(eval("(string->symbol \".\")", env)));
        assertEquals("test.", Writer.write(eval("(string->symbol \"test.\")", env)))
        assertEquals("|#|", Writer.write(eval("(string->symbol \"#\")", env)))
        assertEquals("|#123|", Writer.write(eval("(string->symbol \"#123\")", env)))
        assertEquals("|#abc|", Writer.write(eval("(string->symbol \"#abc\")", env)))
        assertEquals("a#bc", Writer.write(eval("(string->symbol \"a#bc\")", env)))
        assertEquals("abc#", Writer.write(eval("(string->symbol \"abc#\")", env)))
        assertEquals("#%abc", Writer.write(eval("(string->symbol \"#%abc\")", env)))
        assertEquals("(|a b c|)", Writer.write(eval("(list (string->symbol \"a b c\"))", env)))
        assertEquals(Symbol.intern("test"), eval("(string->symbol (symbol->string 'test))", env))
        try {
            eval("(symbol->string 1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("symbol->string: type mismatch; (expected: Symbol, given: 1)", e.message)
        }

        try {
            eval("(string->symbol 1)", env)
            fail()
        } catch (e: IllegalArgumentException) {
            assertEquals("string->symbol: type mismatch; (expected: String, given: 1)", e.message)
        }
    }

    @Test
    fun testStringInterning() {
        // immutable interned strings must have the same hashcode
        assertEquals(true, eval("(apply = (map hashcode '(\"test\" \"test\" \"test\" \"test\" \"test\")))", env))

        // immutable interned strings must point to the same object
        assertEquals(true, eval("(eq? \"test\" \"test\" \"test\" \"test\" \"test\")", env))
        assertEquals(true, eval("(eqv? \"test\" \"test\" \"test\" \"test\" \"test\")", env))
        assertEquals(true, eval("(equal? \"test\" \"test\" \"test\" \"test\" \"test\")", env))
        // mutable interned strings
        assertEquals(false, eval("(eq? (string #\\a) (string #\\a))", env))
        assertEquals(false, eval("(eqv? (string #\\a) (string #\\a))", env))
        assertEquals(true, eval("(equal? (string #\\a) (string #\\a))", env))
        assertEquals(true, eval("(equal? \"a\" (string #\\a))", env))
        assertEquals(true, eval("(equal? (string #\\a) \"a\" )", env))
        assertEquals(true, eval("(mutable? (string #\\a))", env))
        assertEquals(false, eval("(immutable? (string #\\a))", env))
        assertEquals(true, eval("(immutable? \"a\")", env))
        assertEquals(false, eval("(mutable? \"a\")", env))
        assertEquals(true, eval("(immutable? (string->immutable-string (string #\\a)))", env))
    }

    @Test
    fun testIsBlank() {
        assertEquals(true, eval("(blank? nil)", env))
        assertEquals(true, eval("(blank? \"\")", env))
        assertEquals(true, eval("(blank? \"      \")", env))
        assertEquals(true, eval("(blank? \" \t\t\r\n    \")", env))
        assertEquals(false, eval("(blank? \" \t\ta\r\n    \")", env))
        assertEquals(false, eval("(blank? \"not-blank\")", env))
    }

    @Test
    fun testSplit() {
        assertEquals(3, (eval("(split \"a,b,c,\" #\",\")", env) as Vector).size.toLong())
        assertEquals(1, (eval("(split \"a,b,c,\" #\"_\")", env) as Vector).size.toLong())
        assertEquals(4, (eval("(split \"a,b,c,\" #\",\" -1)", env) as Vector).size.toLong())
        assertEquals(10, (eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\")", env) as Vector).size.toLong())
        assertEquals(5, (eval("(split \"q1w2e3r4t5y6u7i8o9p0\" #\"\\d+\" 5)", env) as Vector).size.toLong())
    }

    @Test
    fun testJoin() {
        assertEquals("", eval("(join \"\")", env))
        assertEquals(",", eval("(join \",\")", env))
        assertEquals("", eval("(join \",\" [])", env))
        assertEquals("", eval("(join \",\" '())", env))
        assertEquals("", eval("(join \",\" \"\")", env))
        assertEquals("test", eval("(join \"\" [#\\t #\\e #\\s #\\t])", env))
        assertEquals("test", eval("(join \"\" '(#\\t #\\e #\\s #\\t))", env))
        assertEquals("test", eval("(join \"\" \"test\")", env))
        assertEquals("t,e,s,t", eval("(join \",\" [#\\t #\\e #\\s #\\t])", env))
        assertEquals("t,e,s,t", eval("(join \",\" '(#\\t #\\e #\\s #\\t))", env))
        assertEquals("t,e,s,t", eval("(join \",\" \"test\")", env))
        assertEquals("t123e123s123t", eval("(join \"123\" [#\\t #\\e #\\s #\\t])", env))
        assertEquals("t123e123s123t", eval("(join \"123\" '(#\\t #\\e #\\s #\\t))", env))
        assertEquals("t123e123s123t", eval("(join \"123\" \"test\")", env))
    }

    @Test
    fun testReplace() {
        assertEquals("fabulous ddero oo doo", eval("(replace \"fabulous fodder foo food\" #\"f(o+)(\\S+)\" \"$2$1\")", env))
        assertEquals("The color is blue", eval("(replace \"The color is red\" #\"red\" \"blue\")", env))
        assertEquals("Vegeta", eval("(replace \"Vegeta\" #\"Goku\" \"Gohan\")", env))
    }

    @Test
    fun testReplaceFirst() {
        assertEquals("A good night to you, sir.  Good day.", eval("(replace-first \"A good day to you, sir.  Good day.\" #\"day\" \"night\")", env))
        assertEquals("A good day to you, sir.", eval("(replace-first \"A good day to you, sir.\" #\"madam\" \"master\")", env))
        assertEquals("night need not be SHOUTED.", eval("(replace-first \"Day need not be SHOUTED.\" #\"(?i)day\" \"night\")", env))
        assertEquals("name", eval("(replace-first \"/path/to/file/name\" #\"^.*/\" \"\")", env))
        assertEquals("path/to/file/name", eval("(replace-first \"/path/to/file/name\" #\"^.*?/\" \"\")", env))
    }
}
