package main;

import main.core.ast.SCMSymbol;
import main.core.evaluator.Evaluator;
import main.core.evaluator.IEvaluator;
import main.core.parser.IParser;
import main.core.parser.Tokenizer;
import main.environment.DefaultEnvironment;
import main.environment.IEnvironment;

import java.io.IOException;
import java.text.ParseException;
import java.util.concurrent.atomic.AtomicLong;

import static main.core.specialforms.SpecialForm.DEFINE;
import static main.core.specialforms.SpecialForm.SET;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "scheme@(user)> ";

  private static final IParser parser = new Tokenizer();
  private static final IEvaluator evaluator = new Evaluator();

  public static void main(String[] args) throws ParseException, IOException {
    repl(WELCOME, PROMPT, new DefaultEnvironment(evaluator));
  }

  private static SCMSymbol getNextID() {
    return new SCMSymbol("$" + SYM_COUNTER.incrementAndGet());
  }

  private static void repl(String welcomeMessage, String prompt, IEnvironment env) throws IOException {

    System.out.println(welcomeMessage);
    while (true) {
      try {
        System.err.flush();
        System.out.print(prompt);

        // Read, Tokenize, Parse
        Object sexp = parser.parse(System.in);

        // Eval
        Object result = evaluator.eval(sexp, env);
        if (result != null && result != DEFINE && result != SET) {
          // Put result into environment
          SCMSymbol id = getNextID();
          env.put(id, result);

          // Print
          System.out.println(id + " = " + result.toString());
        }
        // TODO Proper Error handling
      } catch (UnsupportedOperationException e) {
        System.err.println(e.getMessage());
      } catch (IllegalArgumentException e) {
        System.err.println(e.getMessage());
      }
    }
  }
}
