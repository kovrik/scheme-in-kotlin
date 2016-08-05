package core.reader;

import core.exceptions.IllegalSyntaxException;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.Quasiquote;
import core.scm.specialforms.Quote;
import core.scm.specialforms.Unquote;
import core.scm.specialforms.UnquoteSplicing;

import java.io.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.ParseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Reader implements IReader {

  private static final SCMSymbol DOT = new SCMSymbol(".");
  private static final String LINE_BREAKS = "\n\f\r";
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

  /* Valid chars for each radix */
  private static final Map<Character, String> RADIX_CHARS = new HashMap<>();
  static {
    RADIX_CHARS.put('b', "+-.01");
    RADIX_CHARS.put('o', "+-.01234567");
    RADIX_CHARS.put('d', "+-.0123456789");
    RADIX_CHARS.put('x', "+-.0123456789abcdefABCDEF");
  }

  public static String charToNamedChar(Character ch) {
    return CODEPOINTS.get(ch);
  }

  private static boolean isValid(int i) {
    return i > -1 && i < 65535;
  }

  private static boolean isExactness(char c) {
    return c == 'i' || c == 'e';
  }

  private static boolean isRadix(char c) {
    return c == 'b' || c == 'o' || c == 'd' || c == 'x';
  }

  /* Check if digit is valid for a number in a specific radix */
  private static boolean isValidForRadix(char c, Character radix) {
    String s = RADIX_CHARS.get(radix);
    if (s == null) {
      throw new IllegalSyntaxException("Bad radix: " + radix);
    }
    return s.indexOf(c) > -1;
  }

  @Override
  public Object read(String string) {
    PushbackReader reader = new PushbackReader(new StringReader(string), 2);
    try {
      Object token = nextToken(reader);
      if (DOT.equals(token)) {
        throw new IllegalSyntaxException("Illegal use of '.'");
      }
      return token;
    } catch (IOException e) {
      e.printStackTrace();
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public Object read(InputStream inputStream) {
    PushbackReader reader = new PushbackReader(new BufferedReader(new InputStreamReader(inputStream)), 2);
    try {
      Object token = nextNonNullToken(reader);
      if (DOT.equals(token)) {
        throw new IllegalSyntaxException("Illegal use of '.'");
      }
      return token;
    } catch (IOException e) {
      e.printStackTrace();
    } catch (ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  /* Skip all null tokens and return the first non-null */
  private static Object nextNonNullToken(PushbackReader reader) throws IOException, ParseException {
    Object token;
    while ((token = nextToken(reader)) == null) {/* Read */};
    return token;
  }

  /**
   * Read next token
   */
  private static Object nextToken(PushbackReader reader) throws IOException, ParseException {
    int i;
    if (!isValid(i = reader.read())) {
      return null;
    }
    char c = (char)i;
    switch (c) {
      case '\'':
        return readQuote(reader, Quote.QUOTE.symbol());
      case '`':
        return readQuote(reader, Quasiquote.QUASIQUOTE.symbol());
      case ',': {
        char next = (char) reader.read();
        if (next == '@') {
          return readQuote(reader, UnquoteSplicing.UNQUOTE_SPLICING.symbol());
        } else {
          reader.unread(next);
          return readQuote(reader, Unquote.UNQUOTE.symbol());
        }
      }
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
        throw new IllegalSyntaxException("Unexpected list terminator: ')'");
      default: {
        if (Character.isWhitespace(c)) {
          while (isValid(c = (char)reader.read()) && Character.isWhitespace(c)) {
            /* Skip whitespaces */
          }
          reader.unread(c);
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
   */
  private static Object readAtom(PushbackReader reader) throws IOException, ParseException {
    char c = (char)reader.read();
    char next = (char)reader.read();
    reader.unread(next);
    /* Decimal number */
    if (isValidForRadix(c, 'd')) {
      // dot?
      if (c == '.' && DELIMITERS.indexOf(next) > -1) {
        return DOT;
      }
      reader.unread(c);
      /* Read identifier, not a number */
      String number = readIdentifier(reader).toString();
      /* Now check if it IS a valid number */
      return preProcessNumber(number, 'e', 'd');
    } else if (c == ';') {
      return readComment(reader);
    } else if (c == '"') {
      return readString(reader);
    } else if (c == '#') {
      Object result = readHash(reader);
      if (result != null) {
        return result;
      }
    } else {
      reader.unread(c);
      return readIdentifier(reader);
    }
    throw new IllegalSyntaxException("Unknown atom!");
  }

  private static Object readHash(PushbackReader reader) throws ParseException, IOException {
    char next = (char)reader.read();
    if (next == '\\') {
      return readCharacter(reader);
    } else if (next == 't' || next == 'T') {
      return SCMBoolean.TRUE;
    } else if (next == 'f' || next == 'F') {
      return SCMBoolean.FALSE;
    } else if (isRadix(next) || isExactness(next)) {
      /* Read radix and/or exactness and a number */
      Character radix = null;
      Character exactness = null;
      /* We know that next char is either radix or exactness
       * So just check which one and set it */
      if (isRadix(next)) {
        radix = next;
      }
      if (isExactness(next)) {
        exactness = next;
      }
      /* Now read next char: should be either # or numeric */
      next = (char)reader.read();
      /* If it is #, then we expect radix or exactness */
      if (next == '#') {
        next = (char) reader.read();
        if (isRadix(next)) {
          /* Met radix twice */
          if (radix != null) {
            throw new IllegalSyntaxException("Bad number!");
          }
          radix = next;
        }
        if (isExactness(next)) {
            /* Met exactness twice */
          if (exactness != null) {
            throw new IllegalSyntaxException("Bad number!");
          }
          exactness = next;
        }
      } else {
        /* Should be the first digit of a number, so unread it */
        reader.unread(next);
      }
      /* Check if we got exactness or radix */
      exactness = (exactness == null) ? 'e' : exactness;
      radix = (radix == null) ? 'd' : radix;

      /* Read identifier, not a number */
      String number = readIdentifier(reader).toString();
      return preProcessNumber(number, exactness, radix);
    }
    return null;
  }

  /* Check if string represents a valid number */
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

  /* Parse string into a number */
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
        return Long.parseLong(number, r) / Math.pow(r.doubleValue(), number.length() - hasSign - dot);
      }
    }
    return Long.parseLong(number, r);
  }

  /**
   * Read a quoted form abbreviation
   *
   * Syntax:
   * <quote> -> '<form>
   * <quasiquote> -> `<form>
   * <unquote> -> ,<form>
   * <unquote-splicing> -> ,@<form>
   */
  private static Object readQuote(PushbackReader reader, SCMSymbol symbol) throws ParseException, IOException {
    List<Object> quote = SCMCons.list(symbol);
    quote.add(nextNonNullToken(reader));
    return quote;
  }

  /**
   * Read identifier
   *
   * Syntax:
   * <identifier> --> <initial> <subsequent>* | <peculiar identifier>
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
    return new SCMSymbol(identifier.toString());
  }

  /**
   * Read a comment
   *
   * Syntax:
   * <comment> --> ;  <all subsequent characters up to a line break>
   */
  private static String readComment(PushbackReader reader) throws IOException {
    int i;
    while (isValid(i = reader.read()) && (LINE_BREAKS.indexOf((char)i) < 0)) {
      /* Read everything until line break */
    }
    /* Comments are ignored, so just return null */
    return null;
  }

  /**
   * Read a Number
   *
   * Syntax:
   * (See above for full syntax)
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

      throw new IllegalSyntaxException("Bad number!");
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
   * <string> --> " <string element>* "
   * <string element> --> <any character other than " or \> | \" | \\
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
          throw new IllegalSyntaxException("Warning: undefined escape sequence in string - probably forgot backslash: #\\>");
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
   * <character> --> #\ <any character> | #\ <character name>
   * <character name> --> space | newline
   */
  private static Character readCharacter(PushbackReader reader) throws ParseException, IOException {
    int i;
    /* Check if it is a codepoint */
    if (isValid(i = reader.read()) && (Character.isDigit((char)i) || ((char)i == 'x'))) {
      char radix = ((char)i == 'x') ? 'x' : 'd';
      if (radix != 'x') {
        reader.unread((char)i);
      }
      String codepoint = readNumber(reader, radix);
      int cp = -1;
      if (radix == 'd') {
        /* Decimal digits, not a codepoint */
        if (codepoint.length() == 1) {
          return codepoint.charAt(0);
        }
        cp = Integer.parseInt(codepoint, 10);
      } else if (radix == 'x') {
        if (codepoint.isEmpty()) {
          return 'x';
        }
        cp = Integer.parseInt(codepoint, 16);
      }
      if (!Character.isValidCodePoint(cp)) {
        throw new IllegalSyntaxException("Bad codepoint: " + cp);
      }
      return (char)cp;
    }
    reader.unread((char)i);

    StringBuilder character = new StringBuilder();
    char c = (char)reader.read();
    do {
      character.append(c);
    } while ((isValid(i = reader.read())) && (DELIMITERS.indexOf(c = (char)i) < 0));
    reader.unread((char)i);

    // <character name>
    if (character.length() > 1) {
      Character namedChar = NAMED_CHARS.get(character.toString());
      if (namedChar == null) {
        throw new IllegalSyntaxException("Error: unknown named character: \"" + character + "\"");
      }
      return namedChar;
    }
    return character.charAt(0);
  }

  /**
   * Read list
   *
   * Syntax:
   * <list> -> (<list_contents>)
   */
  private static SCMCons<Object> readList(PushbackReader reader) throws ParseException, IOException {
    SCMCons<Object> list = SCMCons.NIL;
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
        if (DOT.equals(token)) {
          /* Remember the position */
          dotPos = pos;
        }
        /* Add token read */
        list.add(token);
      }
    }
    /* It was a proper list */
    if (dotPos < 0) {
      return list;
    }

    /* Process improper list */
    if (dotPos != list.size() - 2) {
      throw new IllegalSyntaxException("Error: bad dotted pair form: " + list);
    }
    /* Remove dot */
    list.remove(dotPos);
    /* Convert list into cons */
    Object last = list.get(list.size() - 1);
    Object beforeLast = list.get(list.size() - 2);
    SCMCons<Object> cons = SCMCons.cons(beforeLast, last);
      /* Cons backwars */
    // TODO Do not iterate the same list again?
    for (int n = list.size() - 3; n >= 0; n--) {
      cons = SCMCons.cons(list.get(n), cons);
    }
    return cons;
  }

  /**
   * Read vector
   *
   * Syntax:
   * <vector> -> </vector>#(<vector_contents>)
   */
  private static SCMVector readVector(PushbackReader reader) throws ParseException, IOException {
    return new SCMVector(readList(reader).toArray());
  }
}
