package core.reader

import core.exceptions.IllegalSyntaxException
import core.scm.*
import core.scm.specialforms.*
import core.utils.Utils.getRadixByChar
import core.utils.Utils.isValidForRadix
import core.utils.Utils.preProcessNumber
import java.io.*
import java.util.regex.Pattern

open class Reader : IReader {

    internal lateinit var reader: PushbackReader

    internal constructor()

    constructor(inputStream: InputStream) {
        this.reader = PushbackReader(BufferedReader(InputStreamReader(inputStream)), 1)
    }

    companion object {
        private val DOT = Symbol.intern(Dot.DOT.toString())
        private val DEREF = Symbol.intern("deref")

        private const val LINE_BREAKS = "\n\r"
        private const val WHITESPACES = LINE_BREAKS + "\u000B \t"
        /* <delimiter> --> <whitespace> | ( | ) | " | ; */
        private const val DELIMITERS = WHITESPACES + ":;(){}[],\"\u0000\uffff"

        /* Allowed escape sequences. See: https://docs.oracle.com/javase/tutorial/java/data/characters.html */
        private val ESCAPED = hashMapOf('t'  to '\t',
                                        'b'  to '\b',
                                        'n'  to '\n',
                                        'r'  to '\r',
                                        '"'  to '\"',
                                        '\\' to '\\')

        val NAMED_CHARS: Map<String, Char> = hashMapOf("newline"   to '\n',
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
        private fun isLineBreak(c: Char) = LINE_BREAKS.contains(c)
        fun isRadix(c: Char)     = "bodxBODX".contains(c)
        fun isExact(c: Char)     = (c == 'e'  || c == 'E')
        fun isInexact(c: Char)   = (c == 'i'  || c == 'I')
        fun isExactness(c: Char) = isExact(c) || isInexact(c)
    }

    override fun read(): List<Any> {
        val tokens = ArrayList<Any>()
        try {
            var token = nextToken()
            while (token != null || tokens.isEmpty()) {
                if (token != null) tokens.add(token)
                token = nextToken()
            }
        } catch (e: IOException) {
            e.printStackTrace()
        }
        return tokens
    }

    @Throws(IOException::class)
    private fun readUntilDelimiter(): String {
        val token = StringBuilder()
        var i = reader.read()
        while (isValid(i) && !DELIMITERS.contains(i.toChar())) {
            token.append(i.toChar())
            i = reader.read()
        }
        reader.unread(i.toChar().toInt())
        return token.toString()
    }

    /* Skip all null tokens and return the first non-null */
    @Throws(IOException::class)
    private fun nextNonNullToken(): Any {
        var token = nextToken()
        while (token == null) { token = nextToken() }
        return token
    }

    /**
     * Read next token
     */
    @Throws(IOException::class)
    fun nextToken(): Any? {
        val i = reader.read()
        if (!isValid(i)) {
            return null
        }
        var c = i.toChar()
        /* Skip whitespaces until line break */
        while (isValid(c.toInt()) && Character.isWhitespace(c) && !isLineBreak(c)) { c = reader.read().toChar() }
        /* Check if there is anything to read */
        if (!isValid(c.toInt()) || isLineBreak(c)) {
            return null
        }
        /* Decimal number */
        if (c != '#' && isValidForRadix(c, 10)) {
            /* Read identifier, not a number */
            val number = c + readUntilDelimiter()
            /* Now check if it IS a valid number */
            return preProcessNumber(number, null, 10)
        }
        return when (c) {
            '\'' -> readQuote(c)
            '`'  -> readQuote(c)
            ','  -> readQuote(c)
            '@'  -> readDeref()
            '#'  -> readHash()
            '('  -> readList(true, ')')
            '{'  -> readHashmap()
            '['  -> readVector(']')
            ';'  -> readComment()
            '"'  -> readString()
            ':'  -> readKeyword()
            ')'  -> throw IllegalSyntaxException("read: unexpected list terminator: $c")
            '}'  -> throw IllegalSyntaxException("read: unexpected terminator: $c")
            ']'  -> throw IllegalSyntaxException("read: unexpected vector terminator: $c")
            else -> {
                val s = c + readUntilDelimiter()
                /* Read true and false as #t and #f */
                when (s) {
                    "true"  -> true
                    "false" -> false
                    else    -> Symbol.intern(s)
                }
            }
        }
    }

    @Throws(IOException::class)
    private fun readHash(): Any {
        val c = reader.read().toChar()
        if (c == '(') {
            /* Read Quoted vector #(...) */
            val vector = readVector(')')
            return when {
                vector.isEmpty() -> vector
                else -> Quote.quote(vector)
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
            return readRegex()
        } else if (isRadix(c) || isExactness(c)) {
            /* Read identifier, not a number */
            val number = "#" + c + readUntilDelimiter()
            /* Read radix and/or exactness and a number */
            var radix: Char? = null
            var exactness: Char? = null
            var restNumber = number
            while (restNumber.length > 1 && restNumber[0] == '#') {
                val ch = restNumber[1]
                if (isExactness(ch)) {
                    if (exactness != null) {
                        throw IllegalSyntaxException("read: bad number: $number")
                    }
                    exactness = ch
                    restNumber = restNumber.substring(2)
                    continue
                }
                if (isRadix(ch)) {
                    if (radix != null) {
                        throw IllegalSyntaxException("read: bad number: $number")
                    }
                    radix = ch
                    restNumber = restNumber.substring(2)
                    continue
                }
                break
            }
            if (restNumber.isEmpty() || "+" == restNumber || "-" == restNumber) {
                throw IllegalSyntaxException("read: bad number: $number")
            }
            /* Check if this is a proper number */
            val result = preProcessNumber(restNumber, exactness, getRadixByChar(radix)) as? Number ?: throw IllegalSyntaxException("read: bad number: $number")
            return result
        } else {
            /* Bad hash syntax: read token and throw exception */
            val token = StringBuilder("#")
            if (isValid(c.toInt())) {
                token.append(c)
            }
            if (!Character.isWhitespace(c)) {
                token.append(readUntilDelimiter())
            }
            throw IllegalSyntaxException("read: bad syntax: $token")
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
            '\'' -> Quote.QUOTE_SYMBOL
            '`'  -> Quasiquote.QUASIQUOTE_SYMBOL
            ','  -> {
                val next = reader.read().toChar()
                if (next == '@') {
                    UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL
                } else {
                    reader.unread(next.toInt())
                    Unquote.UNQUOTE_SYMBOL
                }
            }
            else -> throw IllegalSyntaxException("read: unknown quotation type: $c")
        }
        return Cons.list(quote, nextNonNullToken())
    }

    /**
     * Read a comment
     * Syntax:
     * <comment> --> ;  <all subsequent characters up to a line break>
     */
    @Throws(IOException::class)
    private fun readComment(): String? {
        var i = reader.read()
        /* Read everything until line break */
        while (isValid(i) && !isLineBreak(i.toChar())) { i = reader.read() }
        /* Comments are ignored, return null */
        return null
    }

    /**
     * Read a String
     * Always returns immutable String
     * Syntax:
     * <string> --> "<string element>*"
     * <string element> --> <any character other than></any>" or \> | \" | \\
     */
    @Throws(IOException::class)
    private fun readString(): String {
        val string = StringBuilder()
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '"') {
            /* Escaping */
            if (c == '\\') {
                val next = reader.read().toChar()
                /* Unicode followed by a hexadecimal number */
                if (next == 'u' || next == 'U') {
                    reader.unread(next.toInt())
                    val chr = readCharacter()
                    if (chr == next) {
                        throw IllegalSyntaxException("read: no hex digit following \\u in string")
                    }
                    string.append(chr)
                } else {
                    /* Check that escape sequence is valid */
                    val escaped = ESCAPED[next] ?: throw IllegalSyntaxException("read: unknown escape sequence \\$next in string")
                    string.append(escaped)
                }
            } else {
                string.append(c)
            }
            i = reader.read()
            c = i.toChar()
        }
        /* Always intern Strings read by Reader */
        return string.toString().intern()
    }

    @Throws(IOException::class)
    private fun readRegex(): Pattern {
        val regex = StringBuilder()
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '"') {
            regex.append(c)
            if (c == '\\') {
                regex.append(reader.read().toChar())
            }
            i = reader.read()
            c = i.toChar()
        }
        return Pattern.compile(regex.toString())
    }

    /**
     * Read a Character
     * Syntax:
     * <character> --> #\ <any character> | #\ <character name>
     * <character name> --> space | newline
     */
    @Throws(IOException::class)
    private fun readCharacter(): Char {
        val first = reader.read()
        var rest = readUntilDelimiter()
        if (rest.isEmpty()) {
            return first.toChar()
        }
        /* Check if it is a codepoint */
        var radix = 16
        var isCodepoint = first.toChar() == 'u' || first.toChar() == 'U'
        if (Character.isDigit(first.toChar())) {
            radix = 8
            rest = first.toChar() + rest
            isCodepoint = true
        }
        if (isCodepoint && isValidForRadix(rest[0], radix)) {
            val codepoint = preProcessNumber(rest, 'e', radix) as? Number ?: throw IllegalSyntaxException("read: no hex digit following \\u in string")
            return codepoint.toInt().toChar()
        }
        /* Must be a named char */
        val named = first.toChar() + rest
        return when (named) {
            "linefeed" -> '\n'
            else -> NAMED_CHARS[named] ?: throw IllegalSyntaxException("read: bad character constant: #\\$named")
        }
    }

    /**
     * Read list
     * Syntax:
     * <list> -> (<list_contents>)
     */
    @Throws(IOException::class)
    private fun readList(allowImproperList: Boolean, terminator: Char): Cons<*> {
        /* Remember position of a dot (if we meet it) */
        var dotPos = -1
        var i = reader.read()
        var c = i.toChar()
        /* Skip whitespaces */
        while (Character.isWhitespace(c)) { c = reader.read().toChar() }
        if (c == terminator) return Cons.EMPTY
        val list: Cons<Any?> = Cons.list()
        while (isValid(i) && c != terminator) {
            /* Skip whitespaces */
            while (Character.isWhitespace(c)) { c = reader.read().toChar() }
            if (c == terminator) break
            reader.unread(c.toInt())
            val token = nextNonNullToken()
            /* Check if current token is a dot */
            if (DOT == token) {
                if (!allowImproperList || dotPos > -1) {
                    throw IllegalSyntaxException("read: illegal use of '.'")
                }
                /* Remember the dot position */
                dotPos = list.size
                /* Dot Special Form is allowed as the first element of a list */
                if (dotPos == 0) {
                    list.add(DOT)
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
            /* Validate dot position */
            dotPos != list.size - 1 -> throw IllegalSyntaxException("read: illegal use of '.'")
            /* Convert list into cons */
            else -> {
                var cons = Cons.cons<Any?>(list[list.size - 2], list.last)
                (list.size - 3 downTo 0).forEach { n -> cons = Cons.cons(list[n], cons) }
                cons
            }
        }
    }

    /**
     * Read vector
     * Syntax:
     * <vector> -> #(<vector_contents>)
     */
    @Throws(IOException::class)
    private fun readVector(terminator: Char) = MutableVector(readList(false, terminator).toTypedArray())

    /**
     * Read hashmap
     * Syntax:
     * <hashmap> -> {<key1> <value1>, ..., <keyN> <valueN>}
     */
    @Throws(IOException::class)
    private fun readHashmap(): Map<Any?, Any?> {
        val hashmap = InvokableMap()
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (Character.isWhitespace(c) || c == ',') { c = reader.read().toChar() }
            if (c == '}') break
            reader.unread(c.toInt())
            val key = nextToken()

            /* Skip whitespaces and commas */
            c = reader.read().toChar()
            while (Character.isWhitespace(c) || c == ',') { c = reader.read().toChar() }
            if (c == '}') break
            reader.unread(c.toInt())
            val value = nextToken()

            hashmap.put(key, value)
            i = reader.read()
            c = i.toChar()
        }
        return hashmap
    }

    /**
     * Read set
     * Syntax:
     * <set> -> #{<value1>, ..., <valueN>}
     */
    @Throws(IOException::class)
    private fun readSet(): Set<Any?> {
        val set = HashSet<Any?>()
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (Character.isWhitespace(c) || c == ',') { c = reader.read().toChar() }
            if (c == '}') break
            reader.unread(c.toInt())
            set.add(nextToken())
            i = reader.read()
            c = i.toChar()
        }
        return set
    }

    /**
     * Read keyword
     * Syntax:
     * <keyword> -> :<token>
     */
    @Throws(IOException::class)
    private fun readKeyword(): Keyword {
        val s = readUntilDelimiter()
        if (s.isEmpty()) throw IllegalSyntaxException("read: illegal use of :")
        return Keyword.intern(s)
    }

    /**
     * Deref shortcut
     * \@f -> (deref f)
     */
    @Throws(IOException::class)
    private fun readDeref() = Cons.list(DEREF, nextNonNullToken())
}