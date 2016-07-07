package core.parser;

import core.exceptions.UnmatchedDoubleQuoteException;
import core.exceptions.UnmatchedParenException;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.specialforms.SCMSpecialForm;

import java.io.InputStream;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Scanner;

@Deprecated
public class Parser implements IReader {

  public Object read(InputStream inputStream) {

    // TODO BufferedReader?
    // TODO Decouple
    Scanner scanner = new Scanner(inputStream);

    SCMCons tokens = SCMCons.list();
    boolean read = true;
    boolean unmatched = false;
    String previousInput = "";
    Object sexp = null;
    while (read) {

      /* Read */
      String input = scanner.nextLine();
      if (unmatched) {
        input = previousInput + input;
      }
      try {
        /* Tokenize */
        tokens.addAll(tokenize(input));

        /* Parse */
        sexp = readFromTokens(tokens);
        read = false;
      } catch (UnmatchedParenException e) {
        previousInput = input;
        unmatched = true;
        read = true;
      } catch (UnmatchedDoubleQuoteException e) {
        previousInput = input;
        unmatched = true;
        read = true;
      }
    }
    return sexp;
  }

  public Object read(String string) {
    return null;
  }

  private SCMCons tokenize(String input) {

    return SCMCons.list(Arrays.asList(input.replaceAll("\\(", " ( ")
                                           .replaceAll("\\)", " ) ")
                                           .trim().split("\\s+")));
  }

  private Object readFromTokens(SCMCons tokens) {

    if (tokens.isEmpty()) {
      throw new IllegalArgumentException("Empty tokens list!");
    }
    String token = (String)tokens.pop();
    if ("(".equals(token)) {
      /* Process list */
      return readList(tokens);
    } else if (")".equals(token)) {
      throw new IllegalArgumentException("Unexpected )");
    } else if (token.startsWith("\"")) {
      // TODO
      /* String */
      return readString(tokens);
    } else {
      /* Atom */
      return atom(token);
    }
  }

  private Object readList(SCMCons tokens) {

    if (tokens.isEmpty()) {
      throw new UnmatchedParenException("Unmatched left `(`");
    }
    SCMCons nodes = SCMCons.list();
    while (!")".equals(tokens.getFirst())) {
      nodes.add(readFromTokens(tokens));
      if (tokens.isEmpty()) {
        throw new UnmatchedParenException("Unmatched left `(`");
      }
    }
    tokens.pop();
    return nodes;
  }

  private Object readString(SCMCons tokens) {

    if (tokens.isEmpty()) {
      throw new UnmatchedDoubleQuoteException("Unmatched left `\"`");
    }
    StringBuilder sb = new StringBuilder();
    for (Object t : tokens) {
      String token = (String)t;
      sb.append(token);
      if (token.startsWith("\"") || token.endsWith("\"")) {
        return sb.toString();
      }
      if (tokens.isEmpty()) {
        throw new UnmatchedDoubleQuoteException("Unmatched left `\"`");
      }
    }
    return sb.toString();
  }

  // FIXME Performance
  private Object atom(String token) {

    if (token.charAt(0) == ',') {
      /* Meta */
      return token;
    } else if (token.charAt(0) == '"' && token.charAt(token.length() - 1) == '"') {
      /* String */
      return token;
    }
    try {
      // FIXME NumberFormat is not caching numbers?
      // FIXME Regex check
      return NumberFormat.getInstance().parse(token);
    } catch (ParseException e) {
      SCMSpecialForm specialForm = SCMSpecialForm.get(token);
      if (specialForm != null) {
        return specialForm;
      }
      return new SCMSymbol(token);
    }
  }
}
