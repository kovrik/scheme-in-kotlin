package core.parser;

import core.scm.SCMList;
import core.scm.SCMSymbol;
import core.scm.SCMVector;
import core.scm.specialforms.SCMSpecialForm;

import java.io.*;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.List;

public class Tokenizer implements IParser {

  /* R5RS Grammar

  <token> --> [ ]  <identifier> |
              [V]  <boolean>    |
              [ ]  <number>     |
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

  private static final String LINE_BREAKS = "\n\f\r";

  //   <whitespace> --> <space or newline>
  private static final String WHITESPACES = (char)0x0B + " \t" + LINE_BREAKS;

  //   <delimiter> --> <whitespace> | ( | ) | " | ;
  private static final String DELIMITERS = WHITESPACES + "()\";";

  public Object parse(String string) {

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

  public Object parse(InputStream inputStream) {

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

  public static Object nextToken(PushbackReader reader) throws IOException, ParseException {

    int i;
    if ((i = reader.read()) == -1) {
      return null;
    }
    char c = (char) i;
    switch (c) {
      case '\'':
        return readQuote(reader);
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
      default:
        if (LINE_BREAKS.indexOf(c) > -1) {
          return null;
        } else if (Character.isWhitespace(c)) {
          // skip
        } else {
          reader.unread(c);
          return readAtom(reader);
        }
    }
    return null;
  }

  /* Given a String, and an index, get the atom starting at that index */
  private static Object readAtom(PushbackReader reader) throws IOException, ParseException {

    char c = (char) reader.read();
    char next = (char) reader.read();
    reader.unread(next);

    if (Character.isDigit(c) || c == '+' || c == '-' || c == '.') {
      // TODO Clean up
      reader.unread(c);

      String number = readNumber(reader);
      if ((number.indexOf(".") != number.lastIndexOf(".")) ||
          (number.length() == 1 && (number.charAt(0) == '+' || number.charAt(0) == '-'))) {
        // not a number
        return new SCMSymbol(number);
      }
      return NumberFormat.getInstance().parse(number);
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

  private static Object readQuote(PushbackReader reader) throws ParseException, IOException {
    List<Object> quote = new SCMList<Object>();
    quote.add(SCMSpecialForm.QUOTE);
    Object next = nextToken(reader);
    while (next == null) {
      next = nextToken(reader);
    }
    quote.add(next);
    return quote;
  }

  private static Object readIdentifier(PushbackReader reader) throws IOException {

    StringBuilder identifier = new StringBuilder();
    char c = (char)reader.read();
    while (DELIMITERS.indexOf(c) < 0) {
      identifier.append(c);
      c = (char)reader.read();
    }
    reader.unread(c);
    SCMSpecialForm specialForm = SCMSpecialForm.get(identifier.toString());
    if (specialForm != null) {
      return specialForm;
    } else {
      return new SCMSymbol(identifier.toString());
    }
  }

  private static String readComment(PushbackReader reader) throws IOException {

    StringBuilder comment = new StringBuilder();
    char c = (char)reader.read();
    while (c == ' ' || WHITESPACES.indexOf(c) < 0) {
      comment.append(c);
      c = (char)reader.read();
    }
    return comment.toString();
  }

  // TODO Radix
  // TODO Number types
  //   <digit> --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  // TODO Return NUMBER
  private static String readNumber(PushbackReader reader) throws ParseException, IOException {

    StringBuilder number = new StringBuilder();
    char c = (char)reader.read();
    if (c == '.') {
      number.append('0');
    }
    while (Character.isDigit(c) || c == '.' || c == '+' || c == '-') {
      number.append(c);
      c = (char)reader.read();
    }
    reader.unread(c);
    if (number.charAt(number.length() - 1) == '.') {
      number.append('0');
    }
    return number.toString();
  }

  //   <string> --> " <string element>* "
  //   <string element> --> <any character other than " or \> | \" | \\
  private static String readString(PushbackReader reader) throws ParseException, IOException {

    StringBuilder string = new StringBuilder();
//    string.append('"');
    char c;
    while ((c = (char)reader.read()) != '"') {
      // escaping
      if (c == '\\') {
        char next = (char)reader.read();
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

  //   <character> --> #\ <any character> | #\ <character name>
  //   <character name> --> space | newline
  private static Character readCharacter(PushbackReader reader) throws ParseException, IOException {

    StringBuilder character = new StringBuilder();
    char c;
    while (DELIMITERS.indexOf(c = (char) reader.read()) < 0) {
      character.append(c);
    }
    reader.unread(c);
    // <character name>
    if (character.length() > 1) {
      if ("space".equals(character.toString())) {
        return ' ';
      } else  if ("newline".equals(character.toString())) {
        return '\n';
      } else {
        throw new IllegalArgumentException("Error: unknown named character: \"" + character + "\"");
      }
    }
    return character.charAt(0);
  }

  private static List<Object> readList(PushbackReader reader) throws ParseException, IOException {

    List<Object> list = new SCMList<Object>();
    char c;
    while ((c = (char)reader.read()) != ')') {
      reader.unread(c);
      Object token = nextToken(reader);
      if (token != null) {
        list.add(token);
      }
    }
    return list;
  }

  private static SCMVector readVector(PushbackReader reader) throws ParseException, IOException {

    List<Object> list = readList(reader);
    return new SCMVector(list.toArray(new Object[list.size()]));
  }
}
