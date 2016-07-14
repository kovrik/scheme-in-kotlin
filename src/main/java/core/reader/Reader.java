package core.reader;

import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;

import java.io.*;
import java.math.BigDecimal;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.List;

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
  private static final String DELIMITERS = WHITESPACES + "()\";";

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
  public static Object nextToken(PushbackReader reader) throws IOException, ParseException {
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

    if (Character.isDigit(c) || c == '+' || c == '-' || c == '.') {
      // dot?
      if (c == '.' && DELIMITERS.indexOf(next) > -1) {
        return SCMSpecialForm.DOT;
      }
      reader.unread(c);

      String number = readNumber(reader);
      if ((number.indexOf(".") != number.lastIndexOf(".")) ||
          (number.length() == 1 && (number.charAt(0) == '+' || number.charAt(0) == '-'))) {
        // not a number
        return new SCMSymbol(number);
      }
      Number result = NumberFormat.getInstance().parse(number);
      /* Switch to BigDecimal if number has 19 or more digits */
      // TODO BigInt?
      if (number.length() >= 19)  {
        return new BigDecimal(number);
      }
      if ((result instanceof Double) && (Double.isInfinite((Double)result)))  {
        return new BigDecimal(number);
      }
      if (number.indexOf('.') > -1) {
        return result.doubleValue();
      }
      return result;
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
        //   <boolean> --> #t | #f
        return new SCMSymbol("#" + (char)reader.read());
      } else if (next == 'i' || next == 'e') {
        char exactness = (char) reader.read();
        // TODO
//        String number = "#" + exactness + readNumber(reader);
        String number = readNumber(reader);
        if (number.indexOf(".") != number.lastIndexOf(".")) {
          throw new IllegalArgumentException("Error: illegal number syntax: \"" + number + "\"");
        }
        if (exactness == 'e') {
          return NumberFormat.getInstance().parse(number).longValue();
        } else {
          return NumberFormat.getInstance().parse(number).doubleValue();
        }
      }
    } else {
      reader.unread(c);
      return readIdentifier(reader);
    }
    reader.unread(c);
    throw new IllegalArgumentException("Unknown atom!");
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
  // TODO Radix
  // TODO Number types
  private static String readNumber(PushbackReader reader) throws ParseException, IOException {
    StringBuilder number = new StringBuilder();
    int i = reader.read();
    char c = (char)i;
    if (c == '.') {
      number.append('0');
    }
    while (isValid(i) && Character.isDigit(c) || c == '.' || c == '+' || c == '-') {
      number.append(c);
      i = reader.read();
      c = (char)i;
    }
    reader.unread(c);
    if (number.charAt(number.length() - 1) == '.') {
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
    while ((isValid(i = reader.read())) && (DELIMITERS.indexOf(c = (char)i) < 0)) {
      character.append(c);
    }
    reader.unread((char)i);
    // <character name>
    if (character.length() > 1) {
      if ("space".equals(character.toString())) {
        return ' ';
      } else  if ("newline".equals(character.toString())) {
        // TODO Platform-dependent line separator?
        return '\n';
      } else {
        throw new IllegalArgumentException("Error: unknown named character: \"" + character + "\"");
      }
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
