package core;

import core.environment.DefaultEnvironment;
import core.environment.IEnvironment;
import core.evaluator.Evaluator;
import core.evaluator.IEvaluator;
import core.reader.IReader;
import core.reader.Reader;
import core.scm.SCMSymbol;
import core.scm.specialforms.tco.TailCall;
import core.writer.IWriter;
import core.writer.Writer;

import java.io.IOException;
import java.text.ParseException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static core.scm.SCMUnspecified.UNSPECIFIED;

public class Main {

  private static final AtomicLong SYM_COUNTER = new AtomicLong(0);

  private static final String WELCOME = "Welcome to Scheme in Java!";
  private static final String PROMPT = "> ";

  private static final IWriter writer = new Writer();
  private static final IReader reader = new Reader();
  private static final IEvaluator evaluator = new Evaluator();
  private static final IEnvironment defaultEnvironment = new DefaultEnvironment();

  /* REPL History */
  private static final int MAX_ENTRIES = 10;
  private static final Map HISTORY = new LinkedHashMap(MAX_ENTRIES + 1, 0.75F, true) {
    public boolean removeEldestEntry(Map.Entry eldest) {
      return size() > MAX_ENTRIES;
    }
  };

  public static void main(String[] args) throws ParseException, IOException {
    /* Eval lib procedures */
    for (String proc : defaultEnvironment.getLibraryProcedures()) {
      evaluator.eval(reader.read(proc), defaultEnvironment);
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
        System.out.print(prompt);
        System.out.flush();

        // Read, Tokenize, Parse
        Object sexp = reader.read(System.in);

        // TODO Macroexpand
        Object expanded = macroexpand(sexp);

        // Eval
        Object result = evaluator.eval(expanded, env);

        // TODO Move out of here
        /* TCO: Trampoline */
        while (result instanceof TailCall) {
          result = evaluator.eval(((TailCall)result).getExpr(), ((TailCall) result).getContext());
        }

        if (result != null && result != UNSPECIFIED) {
          // Put result into environment
          SCMSymbol id = getNextID();
          env.put(id, result);
          // Print
          System.out.println(id + " = " + writer.toString(result));
          System.out.flush();
          /* Store sexp in a history */
          HISTORY.put(id, sexp);
        }
        // TODO Proper Error handling
      } catch (Exception e) {
        error(e);
      }
    }
  }

  private static void error(Exception e) {
    System.out.println("ERROR: " + e.getMessage());
    System.out.flush();
  }

  // TODO
  private static Object macroexpand(Object sexp) {
    return sexp;
  }
}
