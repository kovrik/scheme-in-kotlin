package core.reader

import core.exceptions.IllegalSyntaxException
import core.procedures.delayed.Deref
import core.scm.*
import core.scm.MutableSet
import core.scm.specialforms.*
import core.utils.Utils.getRadixByChar
import core.utils.Utils.isValidForRadix
import core.utils.Utils.preProcessNumber
import core.Writer
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PushbackReader
import java.util.regex.Pattern

open class Reader {

    private val name: String = "reader"

    internal lateinit var reader: PushbackReader

    internal constructor()

    constructor(inputStream: InputStream) {
        this.reader = PushbackReader(BufferedReader(InputStreamReader(inputStream)), 1)
    }

    companion object {
        private val deref = Deref()

        private const val LINE_BREAKS = "\n\r"
        private const val WHITESPACES = "$LINE_BREAKS\u000B \t"
        /* <delimiter> --> <whitespace> | ( | ) | " | ; */
        private const val DELIMITERS = "$WHITESPACES:;(){}[],\"\u0000\uffff"
        private const val DELIMITERS_WITH_SLASH = DELIMITERS + '\\'
        private const val RADICES = "bodxBODX"
        private const val EXACT = "eE"
        private const val INEXACT = "iI"

        /* Allowed escape sequences. See: https://docs.oracle.com/javase/tutorial/java/data/characters.html */
        private val ESCAPED = hashMapOf('t'  to '\t',
                'b'  to '\b',
                'n'  to '\n',
                'r'  to '\r',
                '"'  to '\"',
                '\\' to '\\')

        val NAMED_CHARS: Map<String, Char> = hashMapOf("newline"   to '\n',
                "linefeed"  to '\n',
                "space"     to ' ',
                "tab"       to '\t',
                "return"    to '\r',
                "backspace" to '\b',
                "alarm"     to '\u0007',
                "vtab"      to '\u000B',
                "esc"       to '\u001B',
                "escape"    to '\u001B',
                "delete"    to '\u007F',
                "null"      to Character.MIN_VALUE,
                "nul"       to Character.MIN_VALUE)

        private fun isValid(i: Int) = (i > Character.MIN_VALUE.toInt() && i < Character.MAX_VALUE.toInt())
        private fun isLineBreak(c: Char) = c in LINE_BREAKS
        fun isRadix(c: Char)     = c in RADICES
        fun isExact(c: Char)     = c in EXACT
        fun isExactness(c: Char) = c in EXACT + INEXACT
    }

    @Throws(IOException::class)
    private fun readUntilDelimiter(delimiters: String = DELIMITERS) = StringBuilder().apply {
        var i = reader.read()
        while (isValid(i) && i.toChar() !in delimiters) {
            append(i.toChar())
            i = reader.read()
        }
        reader.unread(i.toChar().toInt())
    }.toString()

    /* Return next non-null token */
    @Throws(IOException::class)
    fun read(): Any? {
        var token = nextToken()
        while (token == Unit) { token = nextToken() }
        return token
    }

    /**
     * Read next token
     */
    @Throws(IOException::class)
    fun nextToken(): Any? {
        val i = reader.read()
        var c = i.toChar()
        /* Skip whitespaces until line break */
        while (isValid(c.toInt()) && c.isWhitespace() && !isLineBreak(c)) { c = reader.read().toChar() }
        /* Check if there is anything to read */
        if (!isValid(c.toInt()) || isLineBreak(c)) {
            return Unit
        }
        return when (c) {
            '\'' -> readQuote(c)
            '`'  -> readQuote(c)
            ','  -> readQuote(c)
            '@'  -> readDeref()
            '#'  -> readHash()
            '('  -> readList(true, ')')
            '{'  -> readHashmap()
            '['  -> readVectorMutable(']')
            ';'  -> readComment()
            '"'  -> readString()
            ':'  -> readKeyword()
            ')'  -> throw IllegalSyntaxException("$name: unexpected list terminator: $c")
            '}'  -> throw IllegalSyntaxException("$name: unexpected terminator: $c")
            ']'  -> throw IllegalSyntaxException("$name: unexpected vector terminator: $c")
            else -> (c + readUntilDelimiter()).let {
                when {
                    /* Decimal number */
                    isValidForRadix(c, 10) -> preProcessNumber(it, null, 10)
                    /* Read true and false as #t and #f */
                    it == "true"  -> true
                    it == "false" -> false
                    it == "nil"   -> null
                    it == "null"  -> null
                    else          -> Symbol.intern(it)
                }
            }
        }
    }

    @Throws(IOException::class)
    private fun readHash(): Any {
        val c = reader.read().toChar()
        if (c == '(') {
            /* Read Quoted Immutable Vector #(...) */
            return readVectorImmutable(')').let {
                when {
                    it.isEmpty() -> it
                    else -> Quote.quote(it)
                }
            }
        } else if (c == '{') {
            return readSet()
        } else if (c == '\\') {
            return readCharacter()
        } else if (c == 't' || c == 'T') {
            return true
        } else if (c == 'f' || c == 'F') {
            return false
        } else if (c == '"') {
            return readPattern()
        } else if (c == '\'') {
            return readSyntax()
        } else if (isRadix(c) || isExactness(c)) {
            /* Read identifier, not a number */
            val number = "#" + c + readUntilDelimiter()
            /* Read radix and/or exactness and a number */
            var radix: Char? = null
            var exact: Boolean? = null
            var restNumber = number
            while (restNumber.length > 1 && restNumber[0] == '#') {
                val ch = restNumber[1]
                when {
                    isExactness(ch) -> exact = exact?.let { throw IllegalSyntaxException("$name: bad number: $number") } ?: isExact(ch)
                    isRadix(ch)     -> radix = radix?.let { throw IllegalSyntaxException("$name: bad number: $number") } ?: ch
                }
                restNumber = restNumber.drop(2)
            }
            if (restNumber.isEmpty() || "+" == restNumber || "-" == restNumber) {
                throw IllegalSyntaxException("$name: bad number: $number")
            }
            /* Check if this is a proper number */
            return preProcessNumber(restNumber, exact, getRadixByChar(radix)) as? Number ?:
            throw IllegalSyntaxException("$name: bad number: $number")
        }
        /* Bad hash syntax: read token and throw exception */
        StringBuilder("#").let {
            if (isValid(c.toInt())) { it.append(c) }
            if (!c.isWhitespace()) { it.append(readUntilDelimiter()) }
            throw IllegalSyntaxException("$name: bad syntax: $it")
        }
    }

    /**
     * Read a quoted form abbreviation
     * Syntax:
     * <quote>            -> '<form>
     * <quasiquote>       -> `<form>
     * <unquote>          -> ,<form>
     * <unquote-splicing> -> ,@<form>
     */
    @Throws(IOException::class)
    private fun readQuote(c: Char): List<*> {
        val quote = when (c) {
            '\'' -> Quote.symbol
            '`'  -> Quasiquote.symbol
            ','  -> reader.read().toChar().let {
                when (it) {
                    '@'  -> UnquoteSplicing.symbol
                    else -> {
                        reader.unread(it.toInt())
                        Unquote.symbol
                    }
                }
            }
            else -> throw IllegalSyntaxException("$name: unknown quotation type: $c")
        }
        return listOf(quote, read())
    }

    /**
     * Read a comment
     * Syntax:
     * <comment> --> ;  <all subsequent characters up to a line break>
     *
     * Comments are ignored, always return Unit
     */
    @Throws(IOException::class)
    private fun readComment() = let {
        /* Read everything until line break */
        var i = reader.read()
        while (isValid(i) && !isLineBreak(i.toChar())) { i = reader.read() }
    }

    /**
     * Read a String
     * Always returns immutable String
     * Syntax:
     * <string> --> "<string element>*"
     * <string element> --> <any character other than></any>" or \> | \" | \\
     */
    @Throws(IOException::class)
    private fun readString() = StringBuilder().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '"') {
            /* Escaping */
            if (c == '\\') {
                val next = reader.read().toChar()
                val escaped = ESCAPED[next]
                when {
                    escaped != null -> append(escaped)
                    /* Unicode for the octal number specified by three octal digits */
                    next.isDigit() -> {
                        reader.unread(next.toInt())
                        append(readCharacter())
                    }
                    /* Unicode followed by a hexadecimal number */
                    next in "uUxX" -> {
                        reader.unread(next.toInt())
                        val chr = readCharacter()
                        when (chr) {
                            next -> throw IllegalSyntaxException("$name: no hex digit following \\$next in string")
                            else -> append(chr)
                        }
                    }
                    !next.isLetter() -> append(next)
                    else -> throw IllegalSyntaxException("$name: unknown escape sequence \\$next in string")
                }
            } else {
                append(c)
            }
            i = reader.read()
            c = i.toChar()
        }
        /* Always intern Strings read by Reader */
    }.toString().intern()

    @Throws(IOException::class)
    private fun readPattern() = StringBuilder().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '"') {
            append(c)
            if (c == '\\') {
                append(reader.read().toChar())
            }
            i = reader.read()
            c = i.toChar()
        }
    }.toString().toPattern()

    /**
     * Read a Character
     * Syntax:
     * <character> --> #\ <any character> | #\ <character name>
     * <character name> --> space | newline
     */
    @Throws(IOException::class)
    private fun readCharacter(): Char {
        val first = reader.read()
        var rest = readUntilDelimiter(DELIMITERS_WITH_SLASH)
        if (rest.isEmpty()) {
            return first.toChar()
        }
        /* Check if it is a codepoint */
        var radix = 16
        var isCodepoint = first.toChar() == 'u' || first.toChar() == 'U'
        if (first.toChar().isDigit()) {
            radix = 8
            rest = first.toChar() + rest
            isCodepoint = true
        }
        if (isCodepoint) {
            if (radix == 16 && !isValidForRadix(rest[0], radix)) {
                throw IllegalSyntaxException("$name: no hex digit following \\${first.toChar()} in string")
            }
            val codepoint = preProcessNumber(rest, true, radix) as? Number ?: throw IllegalSyntaxException("$name: bad character constant: #\\$rest")
            return codepoint.toChar()
        }
        /* Must be a named char */
        val named = first.toChar() + rest
        return NAMED_CHARS[named] ?: throw IllegalSyntaxException("$name: bad character constant: #\\$named")
    }

    /**
     * Read list
     * Syntax:
     * <list> -> (<list_contents>)
     */
    @Throws(IOException::class)
    private fun readList(allowImproperList: Boolean, terminator: Char): List<*> {
        /* Remember position of a dot (if we meet it) */
        var dotPos = -1
        var i = reader.read()
        var c = i.toChar()
        /* Skip whitespaces */
        while (c.isWhitespace()) { c = reader.read().toChar() }
        if (c == terminator) return emptyList<Nothing>()
        val list = mutableListOf<Any?>()
        while (isValid(i) && c != terminator) {
            /* Skip whitespaces */
            while (c.isWhitespace()) { c = reader.read().toChar() }
            if (c == terminator) break
            reader.unread(c.toInt())
            val token = read()
            /* Check if current token is a dot */
            if (token == Dot.symbol) {
                if (!allowImproperList || dotPos > -1) {
                    throw IllegalSyntaxException("$name: illegal use of '.'")
                }
                /* Remember the dot position */
                dotPos = list.size
                /* Dot Special Form is allowed as the first element of a list */
                if (dotPos == 0) {
                    list.add(Dot.symbol)
                }
            } else {
                list.add(token)
            }
            i = reader.read()
            c = i.toChar()
        }
        return when {
            /* Was it a proper list or dot is the first element? */
            dotPos < 1 -> list
            else -> {
                /* Validate dot position */
                if (dotPos != list.size - 1) throw IllegalSyntaxException("$name: illegal use of '.'")
                /* Convert list into cons */
                var cons = Cons.cons<Any?>(list[list.size - 2], list.last())
                for (n in list.size - 3 downTo 0) { cons = Cons.cons(list[n], cons) }
                cons
            }
        }
    }

    /**
     * Read mutable vector
     * Syntax:
     * <vector> -> [<vector_contents>]
     */
    @Throws(IOException::class)
    private fun readVectorMutable(terminator: Char) = readList(false, terminator).let {
        when {
            it.isEmpty() -> MutableVector.EMPTY
            else -> MutableVector(it.toTypedArray())
        }
    }

    /**
     * Read immutable vector
     * Syntax:
     * <vector> -> #(<vector_contents>)
     */
    @Throws(IOException::class)
    private fun readVectorImmutable(terminator: Char) = readList(false, terminator).let {
        when {
            it.isEmpty() -> Vector.EMPTY
            else -> Vector(it.toTypedArray())
        }
    }

    /**
     * Read hashmap
     * Syntax:
     * <hashmap> -> {<key1> <value1>, ..., <keyN> <valueN>}
     */
    @Throws(IOException::class)
    private fun readHashmap() = MutableHashmap<Any?, Any?>().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (c.isWhitespace() || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') break
            reader.unread(c.toInt())
            val key = nextToken()

            /* Skip whitespaces and commas */
            c = reader.read().toChar()
            while (c.isWhitespace() || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') throw IllegalSyntaxException("$name: map literal must contain an even number of forms")
            reader.unread(c.toInt())
            val value = nextToken()

            put(key, value)
            i = reader.read()
            c = i.toChar()
        }
    }

    /**
     * Read set
     * Syntax:
     * <set> -> #{<value1>, ..., <valueN>}
     */
    @Throws(IOException::class)
    private fun readSet() = MutableSet<Any?>().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (c.isWhitespace() || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') break
            reader.unread(c.toInt())

            val e = nextToken()
            i = reader.read()
            c = i.toChar()
            while (c.isWhitespace() || c == ',') {
                c = reader.read().toChar()
            }
            if (!set.add(e)) throw IllegalArgumentException("duplicate key: ${Writer.write(e)}")
        }
    }

    /**
     * Read keyword
     * Syntax:
     * <keyword> -> :<token>
     */
    @Throws(IOException::class)
    private fun readKeyword() = readUntilDelimiter().let {
        if (it.isEmpty()) throw IllegalSyntaxException("$name: illegal use of :")
        Keyword.intern(it)
    }

    /**
     * Deref shortcut
     * Syntax:
     * \@f -> (deref f)
     */
    @Throws(IOException::class)
    private fun readDeref() = listOf(deref, read())

    /**
     * Syntax shortcut
     * Syntax:
     * #'template -> (syntax template)
     */
    @Throws(IOException::class)
    private fun readSyntax() = Syntax(read())
}