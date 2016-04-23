package main.core.parser;

import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.exceptions.UnmatchedDoubleQuoteException;
import main.core.exceptions.UnmatchedParenException;
import main.core.specialforms.SpecialForm;

import java.io.InputStream;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Scanner;

public class Parser implements IParser {

  public Object parse(InputStream inputStream) {

    // TODO BufferedReader?
    Scanner scanner = new Scanner(System.in);
    Object sexp = null;

    // TODO Clean up
    SCMList<String> tokens = new SCMList<String>();
    boolean read = true;
    boolean unmatched = false;
    String previousInput = "";
    while (read) {

      // Read
      String input = scanner.nextLine();
      if (unmatched) {
        input = previousInput + input;
      }
      try {
        // Tokenize
        tokens.addAll(tokenize(input));

        // Parse
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

  // TODO Recursive Descent Parser
  private Object parseRD(InputStream inputStream) {

    return null;
  }

  // TODO Replace with Recursive Descent Parser
  private SCMList<String> tokenize(String input) {

    return new SCMList<String>(Arrays.asList(input.replaceAll("\\(", " ( ")
                                                  .replaceAll("\\)", " ) ")
                                                  .trim().split("\\s+")));
  }

  private Object readFromTokens(SCMList<String> tokens) {

    if (tokens.isEmpty()) {
      throw new IllegalArgumentException("Empty tokens list!");
    }
    String token = tokens.pop();
    if ("(".equals(token)) {
      // Process list
      return readList(tokens);
    } else if (")".equals(token)) {
      throw new IllegalArgumentException("Unexpected )");
    } else if (token.startsWith("\"")) {
      // String
      // FIXME TODO
      return readString(tokens);
    } else {
      // Atom
      return atom(token);
    }
  }

  private Object readList(SCMList<String> tokens) {

    if (tokens.isEmpty()) {
      throw new UnmatchedParenException("Unmatched left `(`");
    }
    SCMList<Object> nodes = new SCMList<Object>();
    while (!")".equals(tokens.get(0))) {
      nodes.add(readFromTokens(tokens));
      if (tokens.isEmpty()) {
        throw new UnmatchedParenException("Unmatched left `(`");
      }
    }
    tokens.pop();
    return nodes;
  }

  private Object readString(SCMList<String> tokens) {

    if (tokens.isEmpty()) {
      throw new UnmatchedDoubleQuoteException("Unmatched left `\"`");
    }
    StringBuilder sb = new StringBuilder();
    for (String token : tokens) {

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

    if (token.startsWith(",")) {
      // Meta
      return token;
    } else if (token.charAt(0) == '"' && token.charAt(token.length() - 1) == '"') {
      // String
      return token;
    }
    try {
      // FIXME NumberFormat is not caching numbers?
      // FIXME Regex check
      return NumberFormat.getInstance().parse(token);
    } catch (ParseException e) {
      SpecialForm specialForm = SpecialForm.get(token);
      if (specialForm != null) {
        // FIXME Create static Objects for all KEYWORDS
        return specialForm;
      }
      return new SCMSymbol(token);
    }
  }
}
