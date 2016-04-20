package main;

import main.ast.Node;
import main.ast.SCMList;
import main.environment.DefaultEnvironment;
import main.environment.Environment;
import main.expressions.Keywords;
import main.functions.IFn;
import main.functions.Procedure;
import main.functions.equality.Eqv;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicLong;

import static main.expressions.Keywords.*;

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
  // TODO Store symbols instead of Node objects in Environment
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
        Object result = eval(readFromTokens(tokenize(scanner.nextLine())), env);
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

  private static Node readFromTokens(SCMList<String> tokens) {

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

  private static Node readList(SCMList<String> tokens) {

    SCMList<Node> nodes = new SCMList<Node>();
    while (!")".equals(tokens.get(0))) {
      nodes.add(readFromTokens(tokens));
      if (tokens.isEmpty()) {
        throw new IllegalArgumentException("Unmatched left `(`");
      }
    }
    tokens.pop();
    return new Node(Node.Type.LIST, nodes);
  }

  // FIXME Performance
  private static Node atom(String token) {

    if (token.startsWith(",")) {
      // Meta
      return new Node(Node.Type.META, token);
    } else if (token.charAt(0) == '"' &&
               token.charAt(token.length() - 1) == '"') {
      // String
      return new Node(Node.Type.STRING, token);
    }
    try {
      // FIXME NumberFormat is not caching numbers?
      return new Node(Node.Type.NUMBER, NumberFormat.getInstance().parse(token));
    } catch (ParseException e) {
      Keywords keyword = Keywords.get(token);
      if (keyword != null) {
        // FIXME Create static Nodes for all KEYWORDS
        return new Node(Node.Type.SYMBOL, keyword);
      }
      return new Node(Node.Type.SYMBOL, token);
    }
  }

  // TODO Evaluator class?
  public static Object eval(Node sexp, Environment env) {

    if (sexp.getType() == Node.Type.SYMBOL) {
      return env.find(sexp.getValue());
    } else if (sexp.getType() == Node.Type.META) {
      if (",q".equals(sexp.getValue())) {
        System.out.println("Bye!");
        System.exit(0);
      }
    } else if (sexp.getType() != Node.Type.LIST) {
      return sexp.getValue();
    } else if (sexp.getValue() instanceof SCMList){

      SCMList<Node> list = (SCMList<Node>)sexp.getValue();
      Node op = list.get(0);
      if (QUOTE == op.getValue()) {
        return list.get(1).getValue();
      } else if (IF == op.getValue()) {
        Node test = list.get(1);
        Node consequence = list.get(2);
        Node alternative = list.get(3);
        if ((Boolean)eval(test, env)) {
          return eval(consequence, env);
        } else {
          return eval(alternative, env);
        }
      } else if (COND == op.getValue()) {
        if (list.size() <= 1) {
          throw new IllegalArgumentException("Source expression failed to match any pattern in form (cond)");
        }
        for (int i = 1; i < list.size(); i++) {
          Node node = list.get(i);
          if (node.getType() != Node.Type.LIST) {
            throw new IllegalArgumentException("Invalid clause in subform " + node.getValue());
          }
          List<Node> subform = (List<Node>)node.getValue();
          Node clause = subform.get(0);
          if (ELSE.equals(clause.getValue())) {
            if (i == list.size() - 1) {
              for (int s = 1; i < subform.size() - 1; i++) {
                eval(subform.get(s), env);
              }
              return eval(subform.get(subform.size() - 1), env);
            }
            throw new IllegalArgumentException("cond: else must be the last clause in subform");
          }
          if ((Boolean)eval(clause, env)) {
            for (int s = 1; s < subform.size() - 1; s++) {
              eval(subform.get(s), env);
            }
            return eval(subform.get(subform.size() - 1), env);
          }
        }
      } else if (CASE == op.getValue()) {
        if (list.size() <= 1) {
          throw new IllegalArgumentException("Source expression failed to match any pattern in form (case)");
        }
        Object key = eval(list.get(1), env);
        Eqv eqv = (Eqv)env.find(Keywords.EQV);
        for (int i = 2; i < list.size(); i++) {
          Node node = list.get(i);
          if (node.getType() != Node.Type.LIST) {
            throw new IllegalArgumentException("Invalid clause in subform " + node.getValue());
          }
          List<Node> subform = (List<Node>)node.getValue();
          Node datum = subform.get(0);
          if (ELSE.equals(datum.getValue())) {
            if (i == list.size() - 1) {
              for (int s = 1; s < subform.size() - 1; s++) {
                eval(subform.get(s), env);
              }
              return eval(subform.get(subform.size() - 1), env);
            }
            throw new IllegalArgumentException("cond: else must be the last clause in subform");
          }
          if (datum.getType() != Node.Type.LIST) {
            throw new IllegalArgumentException("Invalid clause in subform " + datum.getValue());
          }
          for (Node n : ((List<Node>) datum.getValue())) {
            if (eqv.apply(key, n.getValue())) {
              for (int s = 1; i < subform.size() - 1; i++) {
                eval(subform.get(s), env);
              }
              return eval(subform.get(subform.size() - 1), env);
            }
          }
        }
      } else if (BEGIN == op.getValue()) {
        for (int i = 1; i < list.size() - 1; i++) {
          eval(list.get(i), env);
        }
        return eval(list.getLast(), env);
      } else if (LET == op.getValue()) {
        if (list.size() < 3) {
          throw new IllegalArgumentException("let: bad let in form");
        }

        Environment localEnv = new Environment(env);
        for (Node node : ((List<Node>) list.get(1).getValue())) {
          Node var = ((List<Node>) node.getValue()).get(0);
          Node init = ((List<Node>) node.getValue()).get(1);
          localEnv.put(var.getValue(), eval(init, env));
        }

        for (int i = 2; i < list.size() - 1; i++) {
          eval(list.get(i), localEnv);
        }
        return eval(list.getLast(), localEnv);
      } else if (LET_SEQ == op.getValue()) {
        if (list.size() < 3) {
          throw new IllegalArgumentException("let*: bad let* in form");
        }

        Environment localEnv = new Environment(env);
        for (Node node : ((List<Node>) list.get(1).getValue())) {
          Node var = ((List<Node>) node.getValue()).get(0);
          Node init = ((List<Node>) node.getValue()).get(1);
          localEnv.put(var.getValue(), eval(init, localEnv));
        }

        for (int i = 2; i < list.size() - 1; i++) {
          eval(list.get(i), localEnv);
        }
        return eval(list.getLast(), localEnv);
      } else if (DEFINE == op.getValue()) {
        // TODO define procedure syntax
        env.put(list.get(1).getValue(), eval(list.get(2), env));
        return DEFINE;
      } else if (SET == op.getValue()) {
        env.find(list.get(1).getValue());
        env.put(list.get(1).getValue(), eval(list.get(2), env));
        return SET;
      } else if (LAMBDA == op.getValue()) {
        // TODO Optimize
        List<Node> paramsNodes = (List<Node>) list.get(1).getValue();
        List<Object> params = new SCMList<Object>();
        for (Node n : paramsNodes) {
          params.add(n.getValue());
        }
        return new Procedure(params, list.get(2), env);
      } else {
        /* Function */
        Object fn = eval(op, env);
        if (fn == null) {
          throw new UnsupportedOperationException("Unbound variable: " + op.getValue());
        }
        Object[] args = new Object[list.size() - 1];
        for (int i = 1; i < list.size(); i++) {
          args[i - 1] = eval(list.get(i), env);
        }
        return ((IFn)fn).invoke(args);
      }
    }
    throw new IllegalArgumentException("Evaluation error: " + sexp);
  }
}
