package main;

import main.ast.SCMList;
import main.ast.SCMSymbol;
import main.core.Evaluator;
import main.core.specialforms.SpecialForm;
import main.environment.DefaultEnvironment;
import main.environment.Environment;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicLong;

import static main.core.specialforms.SpecialForm.DEFINE;
import static main.core.specialforms.SpecialForm.SET;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "scheme@(user)> ";

  public static void main(String[] args) throws ParseException {
    repl(WELCOME, PROMPT, DefaultEnvironment.getEnv());
  }

  private static String getNextID() {
    return "$" + SYM_COUNTER.incrementAndGet();
  }

  // TODO Optimize. Fibonacci. Recursion
  // TODO Reader(s) class
  // TODO Implement booleans properly
  // TODO Numerical comparison operators
  // TODO Keywords to Special Forms (or a separate class)
  // TODO Syntax exception
  // TODO Store symbols instead of Object objects in Environment
  // TODO Comments
  // TODO Immutable vars
  private static void repl(String welcomeMessage, String prompt, Environment env) {

    System.out.println(welcomeMessage);

    // TODO BufferedReader?
    Scanner scanner = new Scanner(System.in);
    while (true) {
      try {

        System.err.flush();
        System.out.print(prompt);

        // TODO Read whole input
        // Read, Tokenize, Eval
        Object result = Evaluator.getInstance().eval(readFromTokens(tokenize(scanner.nextLine())), env);
        if (result != DEFINE && result != SET) {
          // Put result into environment
          String id = getNextID();
          env.put(id, result);
          // Print
          System.out.println(id + " = " + result.toString());
        }
        // FIXME
      } catch (UnsupportedOperationException e) {
        // TODO Proper Error handling
        System.err.println(e.getMessage());
      } catch (IllegalArgumentException e) {
        // TODO Proper Error handling
        System.err.println(e.getMessage());
      }
    }
  }

  // TODO Tokenizer class
  private static SCMList<String> tokenize(String input) {

    return new SCMList<String>(Arrays.asList(input.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").trim().split("\\s+")));
  }

  private static Object readFromTokens(SCMList<String> tokens) {

    if (tokens.isEmpty()) {
      throw new IllegalArgumentException("Empty tokens list!");
    }
    String token = tokens.pop();
    if ("(".equals(token)) {
      return readList(tokens);
    } else if (")".equals(token)) {
      throw new IllegalArgumentException("Unexpected )");
    }
    return atom(token);
  }

  private static Object readList(SCMList<String> tokens) {

    SCMList<Object> nodes = new SCMList<Object>();
    while (!")".equals(tokens.get(0))) {
      nodes.add(readFromTokens(tokens));
      if (tokens.isEmpty()) {
        throw new IllegalArgumentException("Unmatched left `(`");
      }
    }
    tokens.pop();
    return nodes;
  }

  // FIXME Performance
  private static Object atom(String token) {

    if (token.startsWith(",")) {
      // Meta
      return token;
    } else if (token.charAt(0) == '"' &&
               token.charAt(token.length() - 1) == '"') {
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
