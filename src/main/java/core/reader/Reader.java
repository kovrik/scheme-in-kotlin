package core.reader;

import core.exceptions.IllegalSyntaxException;
import core.scm.SCMClass;
import core.scm.SCMCons;
import core.scm.SCMMutableVector;
import core.scm.SCMSymbol;
import core.scm.specialforms.Quasiquote;
import core.scm.specialforms.Quote;
import core.scm.specialforms.Unquote;
import core.scm.specialforms.UnquoteSplicing;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static core.utils.NumberUtils.*;

public class Reader implements IReader {

  // TODO Get rid of this workaround?
  private enum ReadContext {
    DEFAULT,
    HASHMAP;
  }

  static final SCMSymbol DOT = SCMSymbol.of(".");

  private static final String LINE_BREAKS = "\n\f\r";
  private static final String WHITESPACES = LINE_BREAKS + "\u000B \t";
  // <delimiter> --> <whitespace> | ( | ) | " | ;
  private static final String DELIMITERS = WHITESPACES + ";(){},\"\u0000\uffff";
  /* Allowed escape sequences. See: https://docs.racket-lang.org/reference/reader.html#(part._parse-string) */
  private static final String ESCAPE_SEQUENCES = "abtnvefr\"\'\\";

  public static final Map<String, Character> NAMED_CHARS = new HashMap<>();
  static {
    NAMED_CHARS.put("newline",   '\n');
    NAMED_CHARS.put("space",     ' ');
    NAMED_CHARS.put("tab",       '\t');
    NAMED_CHARS.put("return",    '\r');
    NAMED_CHARS.put("backspace", '\b');
    NAMED_CHARS.put("page",      '\f');
    NAMED_CHARS.put("alarm",     '\u0007');
    NAMED_CHARS.put("vtab",      '\u000B');
    NAMED_CHARS.put("esc",       '\u001B');
    NAMED_CHARS.put("escape",    '\u001B');
    NAMED_CHARS.put("delete",    '\u007F');
    NAMED_CHARS.put("null",      Character.MIN_VALUE);
    NAMED_CHARS.put("nul",       Character.MIN_VALUE);
  }

  private static boolean isValid(int i)      { return i > Character.MIN_VALUE && i < Character.MAX_VALUE;}
  private static boolean isLineBreak(char c) { return LINE_BREAKS.indexOf(c) > -1;}
  public  static boolean isRadix(char c)     { return "bodxBODX".indexOf(c) > -1;}
  public  static boolean isExact(char c)     { return c == 'e'   || c == 'E';}
  public  static boolean isInexact(char c)   { return c == 'i'   || c == 'I';}
  public  static boolean isExactness(char c) { return isExact(c) || isInexact(c);}

  PushbackReader reader;

  Reader() {
  }

  public Reader(InputStream inputStream) {
    this.reader = new PushbackReader(new BufferedReader(new InputStreamReader(inputStream)), 1);
  }

  @Override
  public List<Object> read() {
    List<Object> tokens = new ArrayList<>();
    try {
      Object token;
      while (((token = nextToken()) != null) || tokens.isEmpty()) {
        if (DOT.equals(token)) {
          throw new IllegalSyntaxException("read: illegal use of '.'");
        }
        if (token != null) {
          tokens.add(token);
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return tokens;
  }

  private String readUntilDelimiter() throws IOException {
    StringBuilder token = new StringBuilder();
    int i;
    while (isValid(i = reader.read()) && DELIMITERS.indexOf((char)i) < 0) {
      token.append((char)i);
    }
    reader.unread((char)i);
    return token.toString();
  }

  /* Skip all null tokens and return the first non-null */
  private Object nextNonNullToken() throws IOException {
    Object token;
    while ((token = nextToken()) == null) {/* Read */}
    return token;
  }

  Object nextToken() throws IOException {
    return nextToken(ReadContext.DEFAULT);
  }

  /**
   * Read next token
   */
  Object nextToken(ReadContext context) throws IOException {
    int i;
    if (!isValid(i = reader.read())) {
      return null;
    }
    char c = (char)i;
    /* Skip whitespaces until line break */
    if (Character.isWhitespace(c)) {
      while (isValid((int)c) && Character.isWhitespace(c) && !isLineBreak(c)) {
        c = (char) reader.read();
      }
    }
    /* Check if there is anything to read */
    if (!isValid((int)c) || isLineBreak(c)) {
      return null;
    }
    /* Decimal number */
    if (c != '#' && isValidForRadix(c, 10)) {
      /* Read identifier, not a number */
      String number = c + readUntilDelimiter();
      /* Now check if it IS a valid number */
      return preProcessNumber(number, null, 10);
    }
    switch (c) {
      case '\'': return readQuote(c);
      case '`':  return readQuote(c);
      case ',': {
        if (context == ReadContext.DEFAULT) {
          return readQuote(c);
        } else if (context == ReadContext.HASHMAP) {
          return null;
        }
      }
      case '#':  return readHash();
      case '(':  return readList(true);
      case '{':  return readHashmap();
      case ';':  return readComment();
      case '"':  return readString();
      case ')':  throw new IllegalSyntaxException("read: unexpected list terminator: " + c);
      case '}':  throw new IllegalSyntaxException("read: unexpected hashmap terminator: " + c);
      default:   return SCMSymbol.of(c + readUntilDelimiter());
    }
  }

  private Object readHash() throws IOException {
    char c = (char) reader.read();
    if (c == '(') {
      return readVector();
    } else if (c == '\\') {
      return readCharacter();
    } else if (c == 't' || c == 'T') {
      return Boolean.TRUE;
    } else if (c == 'f' || c == 'F') {
      return Boolean.FALSE;
    } else if (isRadix(c) || isExactness(c)) {
      /* Read identifier, not a number */
      String number = "#" + c + readUntilDelimiter();
      /* Read radix and/or exactness and a number */
      Character radix = null;
      Character exactness = null;
      String restNumber = number;
      while (restNumber.length() > 1 && restNumber.charAt(0) == '#') {
        char ch = restNumber.charAt(1);
        if (isExactness(ch)) {
          if (exactness != null) {
            throw new IllegalSyntaxException("read: bad number: " + number);
          }
          exactness = ch;
          restNumber = restNumber.substring(2);
          continue;
        }
        if (isRadix(ch)) {
          if (radix != null) {
            throw new IllegalSyntaxException("read: bad number: " + number);
          }
          radix = ch;
          restNumber = restNumber.substring(2);
          continue;
        }
        break;
      }

      if (restNumber.isEmpty() || "+".equals(restNumber) || "-".equals(restNumber)) {
        throw new IllegalSyntaxException("read: bad number: " + number);
      }

      /* Check if this is a proper number */
      Object result = preProcessNumber(restNumber, exactness, getRadixByChar(radix));
      if (!(result instanceof Number)) {
        throw new IllegalSyntaxException("read: bad number: " + number);
      }
      return result;
    }
    /* Bad hash syntax: read token and throw exception */
    StringBuilder token = new StringBuilder("#");
    if (isValid((int)c)) {
      token.append(c);
    }
    if (!Character.isWhitespace(c)) {
      token.append(readUntilDelimiter());
    }
    throw new IllegalSyntaxException("read: bad syntax: " + token.toString());
  }

  /**
   * Read a quoted form abbreviation
   *
   * Syntax:
   * <quote>            -> '<form>
   * <quasiquote>       -> `<form>
   * <unquote>          -> ,<form>
   * <unquote-splicing> -> ,@<form>
   */
  private List readQuote(char c) throws IOException {
    SCMSymbol symbol = null;
    if (c == '\'') {
      symbol = Quote.QUOTE_SYMBOL;
    } else if (c == '`') {
      symbol = Quasiquote.QUASIQUOTE_SYMBOL;
    } else if (c == ',') {
      char next = (char) reader.read();
      if (next == '@') {
        symbol = UnquoteSplicing.UNQUOTE_SPLICING_SYMBOL;
      } else {
        reader.unread(next);
        symbol = Unquote.UNQUOTE_SYMBOL;
      }
    }
    return SCMCons.list(symbol, nextNonNullToken());
  }

  /**
   * Read a comment
   *
   * Syntax:
   * <comment> --> ;  <all subsequent characters up to a line break>
   */
  private String readComment() throws IOException {
    int i;
    while (isValid(i = reader.read()) && !isLineBreak((char)i)) {
      /* Read everything until line break */
    }
    /* Comments are ignored, return null */
    return null;
  }

  /**
   * Read a String
   * Always returns immutable String
   *
   * Syntax:
   * <string> --> "<string element>*"
   * <string element> --> <any character other than " or \> | \" | \\
   */
  private String readString() throws IOException {
    StringBuilder string = new StringBuilder();
    int i;
    char c;
    while ((isValid(i = reader.read())) && (c = (char)i) != '"') {
      /* Escaping */
      if (c == '\\') {
        char next = (char)reader.read();
        /* Unicode followed by a hexadecimal number */
        if (next == 'u' || next == 'U') {
          reader.unread(next);
          Character chr = readCharacter();
          if (chr.equals(next)) {
            throw new IllegalSyntaxException("read: no hex digit following \\u in string");
          }
          string.append(chr);
          continue;
        }
        /* Check that escape sequence is valid */
        if (ESCAPE_SEQUENCES.indexOf(next) < 0) {
          throw new IllegalSyntaxException(String.format("read: unknown escape sequence \\%s in string", next));
        }
        string.append(c).append(next);
        continue;
      }
      string.append(c);
    }
    return string.toString().intern();
  }

  /**
   * Read a Character
   *
   * Syntax:
   * <character> --> #\ <any character> | #\ <character name>
   * <character name> --> space | newline
   */
  private char readCharacter() throws IOException {
    int first = reader.read();
    String rest = readUntilDelimiter();
    if (rest.isEmpty()) {
      return (char)first;
    }
    /* Check if it is a codepoint */
    int radix = 16;
    boolean isCodepoint = ((char)first == 'u') || ((char)first == 'U');
    if (Character.isDigit((char)first)) {
      radix = 8;
      rest = (char)first + rest;
      isCodepoint = true;
    }
    if (!isValidForRadix(rest.charAt(0), radix)) {
      isCodepoint = false;
    }
    if (isCodepoint) {
      Object codepoint = preProcessNumber(rest, 'e', radix);
      if (!(codepoint instanceof Number)) {
        throw new IllegalSyntaxException("read: no hex digit following \\u in string");
      }
      return (char)((Number)codepoint).intValue();
    }
    /* Must be a named char */
    String character = ((char)first) + rest;
    if ("linefeed".equals(character)) {
      return NAMED_CHARS.get("newline");
    }
    Character namedChar = NAMED_CHARS.get(character);
    if (namedChar == null) {
      throw new IllegalSyntaxException("read: bad character constant: #\\" + character);
    }
    return namedChar;
  }

  /**
   * Read list
   *
   * Syntax:
   * <list> -> (<list_contents>)
   */
  private SCMCons<Object> readList(boolean allowImproperList) throws IOException {
    SCMCons<Object> list = SCMCons.NIL;
    /* Remember position of a dot (if we meet it) */
    int dotPos = -1;
    int i;
    char c;
    while (isValid(i = reader.read()) && ((c = (char)i) != ')')) {
      /* Skip whitespaces */
      while (Character.isWhitespace(c)) {
        c = (char)reader.read();
      }
      if (c == ')') {
        break;
      }
      reader.unread(c);
      Object token = nextToken();
      /* Check if current token is a dot */
      if (DOT.equals(token)) {
        if (!allowImproperList || list.isEmpty() || dotPos > -1) {
          throw new IllegalSyntaxException("read: illegal use of '.'");
        }
        /* Remember the dot position */
        dotPos = list.size();
      } else if (token != null) {
        /* List is empty so far */
        if (list.getSCMClass() == SCMClass.NIL) {
          /* Initialize list with the first element (can't modify NIL) */
          list = SCMCons.list(token);
        } else {
          /* Add list element */
          list.add(token);
        }
      }
    }
    /* Was it a proper list? */
    if (dotPos == -1) {
      return list;
    }
    /* Process improper list */
    if (dotPos != list.size() - 1) {
      throw new IllegalSyntaxException("read: illegal use of '.'");
    }
    /* Convert list into cons */
    return list.toCons();
  }

  /**
   * Read vector
   *
   * Syntax:
   * <vector> -> #(<vector_contents>)
   */
  private SCMMutableVector readVector() throws IOException {
    /* Improper lists are not allowed */
    return new SCMMutableVector(readList(false).toArray());
  }

  /**
   * Read hashmap
   *
   * Syntax:
   * <list> -> {<key1> <value1>, ..., <keyN> <valueN>}
   */
  // TODO Check and simplify
  private Map<Object, Object> readHashmap() throws IOException {
    Map<Object, Object> hashmap = new HashMap<>();
    int i;
    char c;
    while (isValid(i = reader.read()) && ((c = (char)i) != '}')) {
      /* Skip whitespaces */
      while (Character.isWhitespace(c)) {
        c = (char)reader.read();
      }
      if (c == '}') {
        break;
      }
      reader.unread(c);
      /* Skip comma */
      if (c == ',') {
        c = (char)reader.read();
      }
      /* Skip whitespaces */
      while (Character.isWhitespace(c)) {
        c = (char)reader.read();
      }
      if (c == '}') {
        break;
      }
      Object key = nextToken(ReadContext.HASHMAP);

      /* Skip whitespaces */
      while (Character.isWhitespace(c)) {
        c = (char)reader.read();
      }
      if (c == '}') {
        break;
      }
      // TODO Raise 'map must have an even number of forms' error in case hashmap is invalid?
      // TODO Ignore trailing comma?
      Object value = nextToken(ReadContext.HASHMAP);
      hashmap.put(key, value);
    }
    return hashmap;
  }
}