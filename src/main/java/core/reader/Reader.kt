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
        private fun isLineBreak(c: Char) = LINE_BREAKS.contains(c)
        fun isRadix(c: Char)     = "bodxBODX".contains(c)
        fun isExact(c: Char)     = (c == 'e'  || c == 'E')
        fun isInexact(c: Char)   = (c == 'i'  || c == 'I')
        fun isExactness(c: Char) = isExact(c) || isInexact(c)
    }

    @Throws(IOException::class)
    private fun readUntilDelimiter() = StringBuilder().apply {
        var i = reader.read()
        while (isValid(i) && !DELIMITERS.contains(i.toChar())) {
            append(i.toChar())
            i = reader.read()
        }
        reader.unread(i.toChar().toInt())
    }.toString()

    /* Return next non-null token */
    @Throws(IOException::class)
    override fun read(): Any {
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
        var c = i.toChar()
        /* Skip whitespaces until line break */
        while (isValid(c.toInt()) && Character.isWhitespace(c) && !isLineBreak(c)) { c = reader.read().toChar() }
        /* Check if there is anything to read */
        if (!isValid(c.toInt()) || isLineBreak(c)) {
            return null
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
            ')'  -> throw IllegalSyntaxException("read: unexpected list terminator: $c")
            '}'  -> throw IllegalSyntaxException("read: unexpected terminator: $c")
            ']'  -> throw IllegalSyntaxException("read: unexpected vector terminator: $c")
            else -> (c + readUntilDelimiter()).let {
                when {
                    /* Decimal number */
                    isValidForRadix(c, 10) -> preProcessNumber(it, null, 10)
                    /* Read true and false as #t and #f */
                    it == "true"           -> true
                    it == "false"          -> false
                    else                   -> Symbol.intern(it)
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
                when {
                    isExactness(ch) -> exactness = exactness?.let { throw IllegalSyntaxException("read: bad number: $number") } ?: ch
                    isRadix(ch)     -> radix = radix?.let { throw IllegalSyntaxException("read: bad number: $number") } ?: ch
                }
                restNumber = restNumber.drop(2)
            }
            if (restNumber.isEmpty() || "+" == restNumber || "-" == restNumber) {
                throw IllegalSyntaxException("read: bad number: $number")
            }
            /* Check if this is a proper number */
            return preProcessNumber(restNumber, exactness, getRadixByChar(radix)) as? Number ?:
                                    throw IllegalSyntaxException("read: bad number: $number")
        }
        /* Bad hash syntax: read token and throw exception */
        StringBuilder("#").let {
            if (isValid(c.toInt())) { it.append(c) }
            if (!Character.isWhitespace(c)) { it.append(readUntilDelimiter()) }
            throw IllegalSyntaxException("read: bad syntax: $it")
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
            else -> throw IllegalSyntaxException("read: unknown quotation type: $c")
        }
        return Cons.list(quote, read())
    }

    /**
     * Read a comment
     * Syntax:
     * <comment> --> ;  <all subsequent characters up to a line break>
     *
     * Comments are ignored, always return null
     */
    @Throws(IOException::class)
    private fun readComment() = null.apply {
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
                /* Unicode followed by a hexadecimal number */
                if (next == 'u' || next == 'U') {
                    reader.unread(next.toInt())
                    val chr = readCharacter()
                    when (chr) {
                        next -> throw IllegalSyntaxException("read: no hex digit following \\u in string")
                        else -> append(chr)
                    }
                } else {
                    /* Check that escape sequence is valid */
                    append(ESCAPED[next] ?:
                            throw IllegalSyntaxException("read: unknown escape sequence \\$next in string"))
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
    private fun readRegex(): Pattern {
        val regex = StringBuilder().apply {
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
        }.toString()
        return Pattern.compile(regex)
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
        if (isCodepoint) {
            if (!isValidForRadix(rest[0], radix)) throw IllegalSyntaxException("read: no hex digit following \\u in string")
            val codepoint = preProcessNumber(rest, 'e', radix) as? Number ?: throw IllegalSyntaxException("read: no hex digit following \\u in string")
            return codepoint.toChar()
        }
        /* Must be a named char */
        val named = first.toChar() + rest
        return NAMED_CHARS[named] ?: throw IllegalSyntaxException("read: bad character constant: #\\$named")
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
            val token = read()
            /* Check if current token is a dot */
            if (token == Dot.symbol) {
                if (!allowImproperList || dotPos > -1) {
                    throw IllegalSyntaxException("read: illegal use of '.'")
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
            /* Validate dot position */
            dotPos != list.size - 1 -> throw IllegalSyntaxException("read: illegal use of '.'")
            /* Convert list into cons */
            else -> {
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
    private fun readVectorMutable(terminator: Char) = MutableVector(readList(false, terminator).toTypedArray())

    /**
     * Read immutable vector
     * Syntax:
     * <vector> -> #(<vector_contents>)
     */
    @Throws(IOException::class)
    private fun readVectorImmutable(terminator: Char) = Vector(readList(false, terminator).toTypedArray())

    /**
     * Read hashmap
     * Syntax:
     * <hashmap> -> {<key1> <value1>, ..., <keyN> <valueN>}
     */
    @Throws(IOException::class)
    private fun readHashmap() = Hashmap().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (Character.isWhitespace(c) || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') break
            reader.unread(c.toInt())
            val key = nextToken()

            /* Skip whitespaces and commas */
            c = reader.read().toChar()
            while (Character.isWhitespace(c) || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') break
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
    private fun readSet() = HashSet<Any?>().apply {
        var i = reader.read()
        var c = i.toChar()
        while (isValid(i) && c != '}') {
            /* Skip whitespaces and commas */
            while (Character.isWhitespace(c) || c == ',') {
                c = reader.read().toChar()
            }
            if (c == '}') break
            reader.unread(c.toInt())
            add(nextToken())
            i = reader.read()
            c = i.toChar()
        }
    }

    /**
     * Read keyword
     * Syntax:
     * <keyword> -> :<token>
     */
    @Throws(IOException::class)
    private fun readKeyword() = readUntilDelimiter().let {
        if (it.isEmpty()) throw IllegalSyntaxException("read: illegal use of :")
        Keyword.intern(it)
    }

    /**
     * Deref shortcut
     * \@f -> (deref f)
     */
    @Throws(IOException::class)
    private fun readDeref() = Cons.list(DEREF, read())
}