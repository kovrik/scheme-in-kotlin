package core.reader;

import core.exceptions.IllegalSyntaxException;
import core.reader.parsers.Result;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMString;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.Quasiquote;
import core.scm.specialforms.Quote;
import core.scm.specialforms.Unquote;
import core.scm.specialforms.UnquoteSplicing;
import core.utils.NumberUtils;

import java.io.*;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static core.utils.NumberUtils.*;

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

  private static final Map<Character, Character> SPECIAL_CHARS = new HashMap<>();
  static {
    SPECIAL_CHARS.put('t',  '\t');
    SPECIAL_CHARS.put('b',  '\b');
    SPECIAL_CHARS.put('n',  '\n');
    SPECIAL_CHARS.put('r',  '\r');
    SPECIAL_CHARS.put('f',  '\f');
    SPECIAL_CHARS.put('\'', '\'');
    SPECIAL_CHARS.put('\"', '\"');
    SPECIAL_CHARS.put('\\', '\\');
  }

  private static final Map<Character, String> CODEPOINTS = new HashMap<>();
  static {
    for (Map.Entry<String, Character> entry : NAMED_CHARS.entrySet()) {
      CODEPOINTS.put(entry.getValue(), entry.getKey());
    }
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
    return "bodxBODX".indexOf(c) > -1;
  }

  @Override
  public Object readFirst(String string) {
    try (PushbackReader reader = new PushbackReader(new StringReader(string), 2)) {
      Object token = nextToken(reader);
      if (DOT.equals(token)) {
        throw new IllegalSyntaxException("Illegal use of '.'");
      }
      return token;
    } catch (IOException | ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public char readChar(InputStream inputStream) {
    try {
      return (char)inputStream.read();
    } catch (IOException e) {
      e.printStackTrace();
      return 0;
    }
  }

  @Override
  public List<Object> read(String string) {
    try (PushbackReader reader = new PushbackReader(new StringReader(string), 2)) {
      List<Object> tokens = new ArrayList<>();
      Object token;
      while ((token = nextToken(reader)) != null) {
        /* Read */
        if (DOT.equals(token)) {
          throw new IllegalSyntaxException("Illegal use of '.'");
        }
        tokens.add(token);
      }
      return tokens;
    } catch (IOException | ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  @Override
  public List<Object> read(InputStream inputStream) {
    PushbackReader reader = new PushbackReader(new BufferedReader(new InputStreamReader(inputStream)), 2);
    try {
      List<Object> tokens = new ArrayList<>();
      Object token;
      while (((token = nextToken(reader)) != null) || tokens.isEmpty()) {
        /* Read */
        if (DOT.equals(token)) {
          throw new IllegalSyntaxException("Illegal use of '.'");
        }
        if (token != null) {
          tokens.add(token);
        }
      }
      return tokens;
    } catch (IOException | ParseException e) {
      e.printStackTrace();
    }
    return null;
  }

  // TODO cleanup
  @Override
  public List<Object> read(File file) {
    List<Object> tokens = new ArrayList<>();
    try (PushbackReader reader = new PushbackReader(new BufferedReader(new FileReader(file)), 2)) {
      Object token;
      try {
        int read;
        while ((read = reader.read()) != -1) {
          reader.unread(read);
          token = nextToken(reader);
          /* Read */
          if (DOT.equals(token)) {
            throw new IllegalSyntaxException("Illegal use of '.'");
          }
          if (token != null) {
            tokens.add(token);
          }
        }
      } catch (ParseException e) {
        e.printStackTrace();
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return tokens;
  }

  private static String readUntilDelimiter(PushbackReader reader) throws IOException {
    StringBuilder token = new StringBuilder();
    int i;
    while (isValid(i = reader.read()) && (DELIMITERS.indexOf((char)i) < 0)) {
      token.append((char)i);
    }
    reader.unread((char)i);
    return token.toString();
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
    if (LINE_BREAKS.indexOf(c) > -1) {
      return null;
    }
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
          while (isValid(c = (char)reader.read()) && Character.isWhitespace(c) && (LINE_BREAKS.indexOf(c) == -1)) {
            /* Skip whitespaces until line break */
          }
          reader.unread(c);
          return nextToken(reader);
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
    if (c != '#' && isValidForRadix(c, 10)) {
      // dot?
      if (c == '.' && DELIMITERS.indexOf(next) > -1) {
        return DOT;
      }
      reader.unread(c);
      /* Read identifier, not a number */
      String number = readIdentifier(reader).toString();
      if (NumberUtils.SPECIAL_NUMBERS.containsKey(number)) {
        return NumberUtils.SPECIAL_NUMBERS.get(number);
      }
      /* Now check if it IS a valid number */
      return preProcessNumber(number, 'e', 10);
    } else if (c == ';') {
      return readComment(reader);
    } else if (c == '"') {
      return readString(reader);
    } else if (c == '#') {
      return readHash(reader);
    } else {
      reader.unread(c);
      return readIdentifier(reader);
    }
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
      reader.unread(next);
      reader.unread('#');

      /* Read identifier, not a number */
      String number = readIdentifier(reader).toString();

      /* Read radix and/or exactness and a number */
      Character radixChar = null;
      Character exactness = null;
      Result parse = EXACTNESS_RADIX.parse(number);
      if (parse.getType() == Result.Type.SUCCESS) {
        List<String> match = parse.getMatch();
        exactness = match.get(0).charAt(1);
        if (match.size() > 1) {
          radixChar = match.get(1).charAt(1);
        }
      } else {
        parse = RADIX_EXACTNESS.parse(number);
        if (parse.getType() == Result.Type.SUCCESS) {
          List<String> match = parse.getMatch();
          radixChar = match.get(0).charAt(1);
          if (match.size() > 1) {
            exactness = match.get(1).charAt(1);
          }
        }
      }
      exactness = (exactness == null) ? 'e' : exactness;
      radixChar = (radixChar == null) ? 'd' : radixChar;

      String restNumber = parse.getRest();
      if (restNumber.isEmpty() || "+".equals(restNumber) || "-".equals(restNumber)) {
        throw new IllegalSyntaxException("Bad number: no digits!");
      }

      /* Check if this is a proper number */
      Object result = preProcessNumber(restNumber, exactness, getRadixByChar(radixChar));
      if (!(result instanceof Number)) {
        throw new IllegalSyntaxException(String.format("Bad number: %s!", number));
      }
      return result;
    }
    /* Bad hash syntax: read token and throw exception */
    StringBuilder token = new StringBuilder().append('#').append(next).append(readUntilDelimiter(reader));
    throw new IllegalSyntaxException("Bad syntax: " + token);
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
  private static Object readQuote(PushbackReader reader, SCMSymbol symbol) throws ParseException, IOException {
    return SCMCons.list(symbol, nextNonNullToken(reader));
  }

  /**
   * Read identifier
   *
   * Syntax:
   * <identifier> --> <initial> <subsequent>* | <peculiar identifier>
   */
  private static Object readIdentifier(PushbackReader reader) throws IOException {
    return new SCMSymbol(readUntilDelimiter(reader));
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
   * Read a String
   *
   * Syntax:
   * <string> --> " <string element>* "
   * <string element> --> <any character other than " or \> | \" | \\
   */
  private static SCMString readString(PushbackReader reader) throws ParseException, IOException {
    SCMString string = new SCMString();
    int i;
    char c;
    while ((isValid(i = reader.read())) && ((c = (char)i) != '"')) {
      // escaping
      if (c == '\\') {
        i = reader.read();
        char next = (char)i;
        if (next == '>') {
          throw new IllegalSyntaxException("Warning: undefined escape sequence in string - probably forgot backslash: #\\>");
        } else {
          // TODO Unescape in `display` only?
          Character character = SPECIAL_CHARS.get(next);
          if (character != null) {
            string.append(c).append(character);
            continue;
          }
          string.append(c).append(next);
          continue;
        }
      }
      string.append(c);
    }
    return string;
  }

  /**
   * Read a Character
   *
   * Syntax:
   * <character> --> #\ <any character> | #\ <character name>
   * <character name> --> space | newline
   */
  // TODO Implement SRFI-75 instead? u/U or x for Unicode?
  private static Character readCharacter(PushbackReader reader) throws ParseException, IOException {
    int i;
    /* Check if it is a codepoint */
    if (isValid(i = reader.read()) && (Character.isDigit((char)i) || ((char)i == 'u') || ((char)i == 'U'))) {
      /* Hex or Octal? */
      char radixChar;
      if (((char)i == 'u') || ((char)i == 'U')) {
        radixChar = 'x';
      } else {
        radixChar = 'o';
        reader.unread((char) i);
      }

      String identifier = readIdentifier(reader).toString();
      if (identifier.isEmpty()) {
        /* #\x char */
        return 'x';
      }
      int radix = NumberUtils.getRadixByChar(radixChar);
      if (radix == 8  && identifier.length() == 1) {
        return identifier.charAt(0);
      }
      return (char)((Number)preProcessNumber(identifier, 'e', radix)).intValue();
    }

    StringBuilder character = new StringBuilder().append((char)i).append(readUntilDelimiter(reader));
    /* Check if it is a Named Character */
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
    /* Position of a dot (if we have it) */
    int dotPos = -1;
    /* Current index */
    int pos = -1;
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
      Object token = nextToken(reader);
      if (token != null) {
        pos += 1;
        /* Check if current token is a dot */
        if (DOT.equals(token)) {
          /* Remember the position */
          dotPos = pos;
        }
        /* Have no elements in a result list yet */
        if (SCMCons.NIL.equals(list)) {
          /* Create empty list (can't modify NIL) */
          list = SCMCons.list();
        }
        /* Add list element */
        list.add(token);
      }
    }
    /* Was it a proper list? */
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
    /* Cons backwards */
    for (int n = list.size() - 3; n >= 0; n--) {
      cons = SCMCons.cons(list.get(n), cons);
    }
    return cons;
  }

  /**
   * Read vector
   *
   * Syntax:
   * <vector> -> #(<vector_contents>)
   */
  private static SCMVector readVector(PushbackReader reader) throws ParseException, IOException {
    return new SCMVector(readList(reader).toArray());
  }
}
