package core.reader;

import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;

import java.io.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.ParseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Reader implements IReader {

  /* R5RS Grammar

  <token> --> [V]  <identifier> |
              [V]  <boolean>    |
              [V]  <number>     |
              [V]  <character>  |
              [V]  <string>     |
              [V]  (            |
              [V]  )            |
              [V]  #(           |
              [V]  '            |
              [ ]  `            |
              [ ]  ,            |
              [ ]  ,@           |
              [ ]  .

  <comment> --> ;  <all subsequent characters up to a line break>

  <atmosphere> --> <whitespace> | <comment>

  <intertoken space> --> <atmosphere>*

  <identifier> --> <initial> <subsequent>* | <peculiar identifier>

  <initial> --> <letter> | <special initial>

  <letter> --> a | b | c | ... | z

  <special initial> --> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~

  <subsequent> --> <initial> | <digit> | <special subsequent>

  <special subsequent> --> + | - | . | @

  <peculiar identifier> --> + | - | ...

  <syntactic keyword> --> <expression keyword> | else | => | define | unquote | unquote-splicing

  <expression keyword> --> quote | lambda | if | set! | begin | cond | and | or | case | let | let* | letrec | do | delay | quasiquote

  <variable> => <'any <identifier> that isn't also a <syntactic keyword>>

  <number> --> <num 2>| <num 8> | <num 10>| <num 16>

  // NUMBERS -------------------------------------
  TODO

  <num R>      --> <prefix R> <complex R>
  <complex R>  --> <real R> | <real R> @ <real R> | <real R> + <ureal R> i | <real R> - <ureal R> i | <real R> + i | <real R> - i | + <ureal R> i | - <ureal R> i | + i | - i
  <real R>     --> <sign> <ureal R>
  <ureal R>    --> <uinteger R> | <uinteger R> / <uinteger R> | <decimal R>
  <decimal 10> --> <uinteger 10> <suffix> | . <digit 10>+ #* <suffix> | <digit 10>+ . <digit 10>* #* <suffix> | <digit 10>+ #+ . #* <suffix>
  <uinteger R> --> <digit R>+ #*
  <prefix R>   --> <radix R> <exactness> | <exactness> <radix R>
  <suffix>     --> <empty> | <exponent marker> <sign> <digit 10>+
  <exponent marker> --> e | s | f | d | l
  <sign>      --> <empty>  | + |  -
  <exactness> --> <empty> | #i | #e
  <radix 2>   --> #b
  <radix 8>   --> #o
  <radix 10>  --> <empty> | #d
  <radix 16>  --> #x
  <digit 2>   --> 0 | 1
  <digit 8>   --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
  <digit 10>  --> <digit>
  <digit 16>  --> <digit 10> | a | b | c | d | e | f
  */

  // TODO Create Reader for each Type

  private static final String LINE_SEPARATOR = System.getProperty("line.separator");

  private static final String LINE_BREAKS = "\n\f\r";

  // <whitespace> --> <space or newline>
  private static final String WHITESPACES = (char)0x0B + " \t" + LINE_BREAKS;

  // <delimiter> --> <whitespace> | ( | ) | " | ;
  private static final String DELIMITERS = WHITESPACES + "()\";" + '\u0000' + '\uffff';

  private static final Map<String, Character> NAMED_CHARS = new HashMap<>();
  static {
    // TODO Platform-dependent line separator?
    NAMED_CHARS.put("newline",   '\n');
    NAMED_CHARS.put("linefeed",  '\n');
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

  private static final Map<Character, String> CODEPOINTS = new HashMap<>();
  static {
    for (Map.Entry<String, Character> entry : NAMED_CHARS.entrySet()) {
      CODEPOINTS.put(entry.getValue(), entry.getKey());
    }
  }

  private static final Map<Character, Integer> RADICES = new HashMap<>();
  static {
    RADICES.put('b', 2);
    RADICES.put('o', 8);
    RADICES.put('d', 10);
    RADICES.put('x', 16);
  }

  /* Threshold after which we switch to BigDecimals */
  private static final Map<Character, Integer> RADIX_THRESHOLDS = new HashMap<>();
  static {
    RADIX_THRESHOLDS.put('b', 63);
    RADIX_THRESHOLDS.put('o', 21);
    RADIX_THRESHOLDS.put('d', 18);
    RADIX_THRESHOLDS.put('x', 15);
  }

  public static Character namedCharToChar(String named) {
    return NAMED_CHARS.get(named);
  }

  public static String charToNamedChar(Character ch) {
    return CODEPOINTS.get(ch);
  }

  private static boolean isValid(int i) {
    return i > -1 && i != 65535;
  }

  public Object read(String string) {
    PushbackReader reader = new PushbackReader(new StringReader(string), 2);
    try {
      return nextToken(reader);
    } catch (IOException e) {
      e.printStackTrace();
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  public Object read(InputStream inputStream) {
    PushbackReader reader = new PushbackReader(new BufferedReader(new InputStreamReader(inputStream)), 2);
    try {
      return nextToken(reader);
    } catch (IOException e) {
      e.printStackTrace();
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Read next token
   *
   * @param reader
   * @return
   * @throws IOException
   * @throws ParseException
   */
  private static Object nextToken(PushbackReader reader) throws IOException, ParseException {
    int i;
    if ((i = reader.read()) == -1) {
      return null;
    }
    char c = (char) i;
    switch (c) {
      case '\'':
        return readQuote(reader);
      case '`':
        return readQuasiquote(reader);
      case '#':
        char next = (char) reader.read();
        if (next == '(') {
          return readVector(reader);
        } else {
          reader.unread(next);
          reader.unread(c);
          return readAtom(reader);
        }
      case '(':
        return readList(reader);
      case ')':
        throw new IllegalArgumentException("Unexpected list terminator: ')'");
      default: {
        if (Character.isWhitespace(c)) {
          // skip
          return null;
        } else {
          reader.unread(c);
          return readAtom(reader);
        }
      }
    }
  }

  /**
   * Read atom
   *
   * @param reader
   * @return
   * @throws IOException
   * @throws ParseException
   */
  private static Object readAtom(PushbackReader reader) throws IOException, ParseException {

    char c = (char) reader.read();
    char next = (char) reader.read();
    reader.unread(next);

    /* Decimal number */
    if (isValidForRadix(c, 'd')) {
      // dot?
      if (c == '.' && DELIMITERS.indexOf(next) > -1) {
        return SCMSpecialForm.DOT;
      }
      reader.unread(c);
      /* Read identifier, not a number */
      String number = readIdentifier(reader).toString();
      /* Now check if it IS a valid number */
      return preProcessNumber(number, 'e', 'd');
    } else if (c == ';') {
      String comment = readComment(reader);
      return null;
    } else if (c == '"') {
      return readString(reader);
    } else if (c == '#') {
      if (next == '\\') {
        reader.read();
        return readCharacter(reader);
      } else if (next == 't' || next == 'f') {
        // <boolean> --> #t | #f
        return new SCMSymbol("#" + (char) reader.read());
      } else if (isBase(next) || isExactness(next)) {
        /* Read radix and/or exactness and a number */
        Character radix = null;
        Character exactness = null;
        /* We know that next char is either radix or exactness
         * So just check which one and set it */
        if (isBase(next)) {
          radix = (char)reader.read();
        }
        if (isExactness(next)) {
          exactness = (char)reader.read();
        }
        /* Now read next char: should be either # or numeric */
        next = (char) reader.read();
        /* If it is #, then we expect radix or exactness */
        if (next == '#') {
          next = (char) reader.read();
          if (isBase(next)) {
            if (radix != null) {
              /* Met radix twice */
              throw new IllegalArgumentException("Bad number!");
            }
            radix = next;
          }
          if (isExactness(next)) {
            if (exactness != null) {
              /* Met exactness twice */
              throw new IllegalArgumentException("Bad number!");
            }
            exactness = next;
          }
        } else {
          /* Should be the first digit of a number, so unread it */
          reader.unread(next);
        }

        /* Check if we got exactness or radix */
        if (exactness == null) {
          exactness = 'e';
        }
        if (radix == null) {
          radix = 'd';
        }

        /* Read identifier, not a number */
        String number = readIdentifier(reader).toString();
        return preProcessNumber(number, exactness, radix);
      }
    } else {
      reader.unread(c);
      return readIdentifier(reader);
    }
    reader.unread(c);
    throw new IllegalArgumentException("Unknown atom!");
  }

  private static Object preProcessNumber(String number, char exactness, char radix) throws ParseException {

    boolean hasTwoDots = number.indexOf('.') != number.lastIndexOf('.');
    boolean isSignCharOnly = (number.length() == 1) && (number.charAt(0) == '+' || number.charAt(0) == '-');
    boolean hasBadSignPos = (number.lastIndexOf('+') > 0) || (number.lastIndexOf('-') > 0);

    /* Validate all digits */
    boolean allDigitsAreValid = true;
    for (char c : number.toCharArray()){
      if (!isValidForRadix(c, radix)) {
        allDigitsAreValid = false;
        break;
      }
    }
    if (hasTwoDots || isSignCharOnly || hasBadSignPos || !allDigitsAreValid) {
      /* Not a number! */
      return new SCMSymbol(number);
    }

    /* Drop + sign if exists */
    if (number.charAt(0) == '+') {
      number = number.substring(1);
    }

    /* Check exactness */
    // TODO Exactness

    return processNumber(number, radix, exactness);
  }

  private static Number processNumber(String number, char radix, char exactness) {

    int hasSign = (number.charAt(0) == '-') ? 1 : 0;
    int dot = number.indexOf('.');
    Integer r = RADICES.get(radix);
    Integer threshold = RADIX_THRESHOLDS.get(radix);

    if (number.length() > (threshold + hasSign)) {
      if (dot > -1) {
        /* Remove dot */
        number = number.replace(".", "");
        BigInteger bigInteger = new BigInteger(number, r);
        return new BigDecimal(bigInteger).divide(new BigDecimal(r).pow(number.length() - hasSign - dot), MathContext.DECIMAL32);
      }
      BigInteger bigInteger = new BigInteger(number, r);
      return new BigDecimal(bigInteger);
    }
    if (dot > -1) {
      if (r == 10) {
        return Double.parseDouble(number);
      } else {
        /* Remove dot */
        number = number.replace(".", "");
        long l = Long.parseLong(number, r);
        return l / Math.pow(r.doubleValue(), number.length() - hasSign - dot);
      }
    }
    return Long.parseLong(number, r);
  }

  private static boolean isExactness(char c) {
    return c == 'i' || c == 'e';
  }

  private static boolean isBase(char c) {
    return c == 'b' || c == 'o' || c == 'd' || c == 'x';
  }

  /* Check if digit is valid for a number in a specific radix */
  private static boolean isValidForRadix(char c, Character radix) {
    if (radix == null || radix.equals('d')) {
      return "+-.0123456789".indexOf(c) > -1;
    }
    if (radix.equals('b')) {
      return "+-.01".indexOf(c) > -1;
    }
    if (radix.equals('o')) {
      return "+-.01234567".indexOf(c) > -1;
    }
    if (radix.equals('x')) {
      return "+-.0123456789abcdefABCDEF".indexOf(c) > -1;
    }
    throw new IllegalArgumentException("Bad radix: " + radix);
  }

  /**
   * Read a quoted form
   *
   * Syntax:
   *
   * <quote> -> '<form>
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  // FIXME '0e => (quote 0eabc), not (quote 0)
  private static Object readQuote(PushbackReader reader) throws ParseException, IOException {
    List<Object> quote = SCMCons.list(SCMSpecialForm.QUOTE);
    Object next = nextToken(reader);
    while (next == null) {
      next = nextToken(reader);
    }
    quote.add(next);
    return quote;
  }

  /**
   * Read a quasi-quoted form
   *
   * Syntax:
   *
   * <quasiquote> -> `<form>
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  private static Object readQuasiquote(PushbackReader reader) throws ParseException, IOException {
    List<Object> quote = SCMCons.list(SCMSpecialForm.QUASIQUOTE);
    Object next = nextToken(reader);
    while (next == null) {
      next = nextToken(reader);
    }
    quote.add(next);
    return quote;
  }

  /**
   * Read identifier
   *
   * Syntax:
   *
   * <identifier> --> <initial> <subsequent>* | <peculiar identifier>
   *
   * @param reader
   * @return
   * @throws IOException
   */
  private static Object readIdentifier(PushbackReader reader) throws IOException {
    StringBuilder identifier = new StringBuilder();
    int i = reader.read();
    char c = (char)i;
    while (isValid(i) && DELIMITERS.indexOf(c) < 0) {
      identifier.append(c);
      i = reader.read();
      c = (char)i;
    }
    reader.unread(c);
    SCMSpecialForm specialForm = SCMSpecialForm.get(identifier.toString());
    /* Check if it is a Special Form */
    if (specialForm != null) {
      return specialForm;
    } else {
      return new SCMSymbol(identifier.toString());
    }
  }

  /**
   * Read a comment
   *
   * Syntax:
   *
   * <comment> --> ;  <all subsequent characters up to a line break>
   *
   * @param reader
   * @return
   * @throws IOException
   */
  private static String readComment(PushbackReader reader) throws IOException {
    StringBuilder comment = new StringBuilder();
    int i = reader.read();
    char c = (char)i;
    /* Read everything until line break */
    while (isValid(i) && (LINE_BREAKS.indexOf(c) < 0)) {
      comment.append(c);
      i = reader.read();
      c = (char)i;
    }
    return comment.toString();
  }

  /**
   * Read a Number
   *
   * Syntax:
   *
   * (See above for full syntax)
   * <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  private static String readNumber(PushbackReader reader, Character radix) throws ParseException, IOException {
    StringBuilder number = new StringBuilder();
    int i = reader.read();
    char c = (char)i;
    if (c == '.') {
      number.append('0');
    }
    while (isValid(i) && (isValidForRadix(c, radix))) {
      number.append(c);
      i = reader.read();
      c = (char)i;
    }
    /* Ensure we have 1 sign/dot max */
    if ((number.indexOf("+") != number.lastIndexOf("+")) ||
        (number.indexOf("-") != number.lastIndexOf("-")) ||
        (number.indexOf(".") != number.lastIndexOf("."))) {

      throw new IllegalArgumentException("Bad number!");
    }
    reader.unread(c);
    if (number.length() > 0 && number.charAt(number.length() - 1) == '.') {
      number.append('0');
    }
    return number.toString();
  }

  /**
   * Read a String
   *
   * Syntax:
   *
   * <string> --> " <string element>* "
   * <string element> --> <any character other than " or \> | \" | \\
   *
   * @param reader
   * @return String
   * @throws ParseException
   * @throws IOException
   */
  private static String readString(PushbackReader reader) throws ParseException, IOException {
    StringBuilder string = new StringBuilder();
    int i;
    char c;
    while ((isValid(i = reader.read())) && ((c = (char)i) != '"')) {
      // escaping
      if (c == '\\') {
        i = reader.read();
        char next = (char)i;
        if (next == '>') {
          throw new IllegalArgumentException("Warning: undefined escape sequence in string - probably forgot backslash: #\\>");
        } else if (next == '"') {
          string.append(c).append(next);
          continue;
        }
      }
      string.append(c);
    }
    return string.toString();
  }

  /**
   * Read a Character
   *
   * Syntax:
   *
   * <character> --> #\ <any character> | #\ <character name>
   * <character name> --> space | newline
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  private static Character readCharacter(PushbackReader reader) throws ParseException, IOException {
    StringBuilder character = new StringBuilder();
    int i;
    char c;
    /* Check if it is a codepoint */
    if (isValid(i = reader.read()) && (Character.isDigit((char)i) || ((char)i == 'x'))) {
      char radix = ((char)i == 'x') ? 'x' : 'd';
      if (radix != 'x') {
        reader.unread((char)i);
      }
      String codepoint = readNumber(reader, radix);
      int cp = -1;
      if (radix == 'd') {
        /* Decimal digits */
        if (codepoint.length() == 1) {
          return codepoint.charAt(0);
        }
        cp = Integer.parseInt(codepoint);
      } else if (radix == 'x') {
        if (codepoint.isEmpty()) {
          return 'x';
        }
        cp = Integer.parseInt(codepoint, 16);
      }
      if (!Character.isValidCodePoint(cp)) {
        throw new IllegalArgumentException("Bad codepoint: " + cp);
      }
      /* Is it a named char? */
      return (char)cp;
    }
    reader.unread((char)i);

    while ((isValid(i = reader.read())) && (DELIMITERS.indexOf(c = (char)i) < 0)) {
      character.append(c);
    }
    if (character.length () == 0) {
      character.append((char)i);
    } else {
      reader.unread((char) i);
    }
    // <character name>
    if (character.length() > 1) {
      Character namedChar = NAMED_CHARS.get(character.toString());
      if (namedChar == null) {
        throw new IllegalArgumentException("Error: unknown named character: \"" + character + "\"");
      }
      return namedChar;
    }
    return character.charAt(0);
  }

  /**
   * Read list
   *
   * Syntax:
   *
   * <list> -> (<list_contents>)
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  private static List<Object> readList(PushbackReader reader) throws ParseException, IOException {
    List<Object> list = SCMCons.NIL;
    int i;
    char c;
    /* Position of a dot (if we have it) */
    int dotPos = -1;
    /* Current index */
    int pos = -1;
    while (isValid(i = reader.read()) && ((c = (char)i) != ')')) {
      reader.unread(c);
      Object token = nextToken(reader);
      if (token != null) {
        pos += 1;
        /* Have no elements in a result list yet */
        if (SCMCons.NIL.equals(list)) {
          /* Create empty list (can't modify NIL) */
          list = SCMCons.list();
        }
        /* Check if current token is a dot */
        if (SCMSpecialForm.DOT.equals(token)) {
          /* Remember the position */
          dotPos = pos;
        }
        /* Add token read */
        list.add(token);
      }
    }
    // TODO Do not iterate the same list again?
    /* Did we have a dot? */
    if (dotPos < 0) {
      /* No. Return result */
      return list;
    } else {
      /* Process dotted pair notation */
      if (dotPos != list.size() - 2) {
        /* Malformed dotted pair */
        throw new IllegalArgumentException("Error: bad dotted pair form: " + list);
      }
      /* Remove dot */
      list.remove(dotPos);
      /* Convert list into cons */
      Object last = list.get(list.size() - 1);
      Object beforeLast = list.get(list.size() - 2);
      SCMCons<Object> cons = SCMCons.cons(beforeLast, last);
      /* Cons backwars */
      for (int n = list.size() - 3; n >= 0; n--) {
        cons = SCMCons.cons(list.get(n), cons);
      }
      return cons;
    }
  }

  /**
   * Read vector
   *
   * Syntax:
   *
   * <vector> -> </vector>#(<vector_contents>)
   *
   * @param reader
   * @return
   * @throws ParseException
   * @throws IOException
   */
  private static SCMVector readVector(PushbackReader reader) throws ParseException, IOException {
    List<Object> list = readList(reader);
    return new SCMVector(list.toArray());
  }
}
