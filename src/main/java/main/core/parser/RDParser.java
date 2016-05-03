package main.core.parser;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.specialforms.SpecialForm;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.List;
import java.util.regex.Pattern;

@Deprecated
public class RDParser extends PushbackReader {

  public RDParser(Reader in) {
    super(in);
  }

  private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("^\\w+$");

  private static final Pattern META_PATTERN = Pattern.compile("^\\w+$");
  private static final Pattern NUMBER_PATTERN = Pattern.compile("^[-+]?\\d+\\.?\\d*$");
  private static final String WHITESPACES = " \t\n\\x0B\f\r]";

  public Object parse() throws IOException, ParseException {

    int i = read();
    if (i == -1) {
      return null;
    }
    char c = (char) i;
    if (c == '(') {
      /* List */
      return readList();
    } else if (c == ')') {
      throw new IllegalArgumentException("Unexpected ')'");
    } else if (WHITESPACES.indexOf(c) > -1) {
      // skip whitespaces
      while (WHITESPACES.indexOf(c = (char)read()) > -1) {
        // do nothing
      }
      unread(c);
      return null;
    } else {
      return readAtom(c);
    }
  }

  private Object readAtom(char c) throws IOException, ParseException {

    if (c == ',') {
      /* Meta */
      return c + readMeta();
    } else if (c == '"') {
      /* String */
      return c + readString();
    } else if (c == '+' || c == '-' || c == '.' || Character.isDigit(c)) {
      /* Number */
      return NumberFormat.getInstance().parse(c + readNumber());
    } else {
      // FIXME
      unread(c);
      String token = readPattern(IDENTIFIER_PATTERN);
      SpecialForm specialForm = SpecialForm.get(token);
      if (specialForm != null) {
        return specialForm;
      }
      return new SCMSymbol(token);
    }
  }

  private String readPattern(Pattern pattern) throws IOException {

    StringBuilder sb = new StringBuilder();
    char c;
    do {
      c = (char)read();
      sb.append(c);
    } while (pattern.matcher(sb.toString()).matches());
    unread(c);
    return sb.substring(0, sb.length() - 1);
  }

  private String readNumber() throws IOException {
    return readPattern(NUMBER_PATTERN);
  }

  private String readMeta() throws IOException {
    return readPattern(META_PATTERN);
  }

  private String readString() throws IOException {
    StringBuilder sb = new StringBuilder();
    char c;
    char p = Character.MIN_VALUE;
    while (!(((c = (char)read()) == '"') && (p != '\\'))) {
      sb.append(c);
      p = c;
    }
    return sb.append('"').toString();
  }

  private List<Object> readList() throws IOException, ParseException {

    SCMList<Object> list = new SCMList<Object>();
    Object token;
    char c;
    while ((c = (char)read()) != ')') {
      if (WHITESPACES.indexOf(c) > -1) {
        continue;
      }
      unread(c);
      token = parse();
      if (token != null && !token.toString().trim().isEmpty()) {
        list.add(token);
      }
    }
    return list;
  }
}
