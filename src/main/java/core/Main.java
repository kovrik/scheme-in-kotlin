package core;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.parser.IParser;
import core.parser.Tokenizer;
import core.scm.SCMSymbol;
import core.scm.errors.SCMError;

import java.io.IOException;
import java.text.ParseException;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static core.scm.specialforms.SCMSpecialForm.DEFINE;
import static core.scm.specialforms.SCMSpecialForm.SET;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "scheme@(user)> ";

  private static final IParser parser = new Tokenizer();
  private static final IEvaluator evaluator = new Evaluator();
  private static final IEnvironment defaultEnvironment = new DefaultEnvironment();

  public static void main(String[] args) throws ParseException, IOException {

    /* Eval lib procedures */
    for (Map.Entry<String, String> entry : ((DefaultEnvironment)defaultEnvironment).getProcs().entrySet()) {
      defaultEnvironment.put(entry.getKey(), evaluator.eval(parser.parse(entry.getValue()), defaultEnvironment));
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

        // Read, Tokenize, Parse
        Object sexp = parser.parse(System.in);

        // Eval
        Object result = evaluator.eval(sexp, env);
        if (result != null && result != DEFINE && result != SET) {
          // Put result into environment
          SCMSymbol id = getNextID();
          env.put(id, result);

          // Print
          if (result instanceof String) {
            System.out.println(id + " = \"" + result + "\"");
          } else {
            System.out.println(id + " = " + result);
          }
        }
        // TODO Proper Error handling
      } catch (UnsupportedOperationException e) {
        System.err.println(e.getMessage());
      } catch (IllegalArgumentException e) {
        System.err.println(e.getMessage());
      } catch (SCMError e) {
        System.err.println(e.getMessage());
      }
    }
  }
}
