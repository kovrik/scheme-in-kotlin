package core;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.parser.IReader;
import core.parser.Reader;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.errors.SCMError;

import java.io.IOException;
import java.text.ParseException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static core.scm.specialforms.SCMSpecialForm.UNSPECIFIED;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "scheme@(user)> ";

  private static final IReader reader = new Reader();
  private static final IEvaluator evaluator = new Evaluator();
  private static final IEnvironment defaultEnvironment = new DefaultEnvironment();

  public static void main(String[] args) throws ParseException, IOException {

    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : ((DefaultEnvironment)defaultEnvironment).getProcs().entrySet()) {
      defaultEnvironment.put(entry.getKey(), evaluator.eval(reader.read(entry.getValue()), defaultEnvironment));
    }
    repl(WELCOME, PROMPT, defaultEnvironment);
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
        System.out.flush();

        // Read, Tokenize, Parse
        Object sexp = reader.read(System.in);

        // TODO Macroexpand
        Object expanded = macroexpand(sexp);

        // Eval
        Object result = evaluator.eval(expanded, env);
        if (result != null && result != UNSPECIFIED) {
          // Put result into environment
          SCMSymbol id = getNextID();
          env.put(id, result);

          // Print
          if (result instanceof String) {
            System.out.println(id + " = \"" + result + "\"");
          } else if (result instanceof List) {
            System.out.println(id + " = " + SCMCons.toString((List) result));
          } else {
            System.out.println(id + " = " + result);
          }
          System.out.flush();
        }
        // TODO Proper Error handling
      } catch (UnsupportedOperationException e) {
        error(e);
      } catch (IllegalArgumentException e) {
        error(e);
      } catch (ArithmeticException e) {
        error(e);
      } catch (SCMError e) {
        error(e);
      }
    }
  }

  private static void error(Exception e) {
    System.err.println("ERROR: " + e.getMessage());
    System.err.flush();
  }

  // TODO
  private static Object macroexpand(Object sexp) {
    return sexp;
  }
}
