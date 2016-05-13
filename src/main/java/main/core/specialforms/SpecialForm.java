package main.core.specialforms;

import main.core.ast.SCMBoolean;
import main.core.ast.SCMList;
import main.core.ast.SCMSymbol;
import main.core.environment.Environment;
import main.core.environment.IEnvironment;
import main.core.evaluator.IEvaluator;
import main.core.exceptions.ArityException;
import main.core.procedures.equivalence.Eqv;
import main.core.procedures.Procedure;
import main.core.procedures.delayed.Promise;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum SpecialForm implements ISpecialForm {

  // FIXME
  UNDEFINED("UNDEFINED") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new IllegalArgumentException("Can't evaluate UNDEFINED form!");
    }
  },
  /* Fundamental forms */
  DEFINE("define") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object definition = expression.get(1);
      Object value = expression.get(2);
      if (definition instanceof SCMSymbol) {
        env.put(definition, evaluator.eval(value, env));
      } else if (definition instanceof SCMList) {
        if (((SCMList) definition).isEmpty()) {
          throw new IllegalArgumentException("Wrong lambda syntax!");
        }
        env.put(((SCMList<Object>)definition).pop(), new Procedure((SCMList<Object>)definition, value));
      } else {
        throw new IllegalArgumentException("Wrong define syntax!");
      }
      return DEFINE;
    }
  },
  LAMBDA("lambda") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return new Procedure((List<Object>)expression.get(1), expression.get(2));
    }
  },
  IF("if") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object test = expression.get(1);
      Object consequence = expression.get(2);
      Object alternative = expression.get(3);
      if (SCMBoolean.valueOf(evaluator.eval(test, env))) {
        return evaluator.eval(consequence, env);
      } else {
        return evaluator.eval(alternative, env);
      }
    }
  },
  WHEN("when") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new ArityException(0, getSyntax());
      }
      Object test = expression.get(1);
      if (!SCMBoolean.valueOf(evaluator.eval(test, env))) {
        return null;
      } else {
        if (expression.size() > 1) {
          Object result = null;
          for (int i = 2; i < expression.size(); i++) {
            result = evaluator.eval(expression.get(i), env);
          }
          return result;
        }
        return null;
      }
    }
  },
  QUOTE("quote") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return expression.get(1);
    }
  },
  UNQUOTE("unquote") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  UNQUOTE_SPLICING("unquote-splicing") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  QUASIQUOTE("quasiquote") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  DEFINE_SYNTAX("define-syntax") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LET_SYNTAX("let-syntax") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LETREC_SYNTAX("letrec-syntax") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  SYNTAX_RULES("syntax-rules") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  SET("set!") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      env.find(expression.get(1));
      env.put(expression.get(1), evaluator.eval(expression.get(2), env));
      return SET;
    }
  },
  /* Library forms */
  DO("do") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LET("let") {

    // TODO: Named let

    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("let: bad let in form");
      }

      IEnvironment localEnv = new Environment(env);
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        Object init = ((List<Object>) node).get(1);
        if (localEnv.get(var) != null) {
          throw new IllegalArgumentException("let: duplicate bound variable " + var);
        }
        localEnv.put(var, evaluator.eval(init, env));
      }

      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      return evaluator.eval(expression.getLast(), localEnv);
    }
  },
  LET_S("let*") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("let*: bad let* in form");
      }

      IEnvironment localEnv = new Environment(env);
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        Object init = ((List<Object>) node).get(1);
        localEnv.put(var, evaluator.eval(init, localEnv));
      }

      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      return evaluator.eval(expression.getLast(), localEnv);
    }
  },
  LETREC("letrec") {
    /*
     * TODO:
     * One restriction on letrec is very important:
     * it must be possible to evaluate each <init> without assigning or referring to the value of any <variable>.
     * If this restriction is violated, then it is an error.
     * The restriction is necessary because Scheme passes arguments by value rather than by name.
     * In the most common uses of letrec, all the <init>s are lambda expressions and the restriction is satisfied automatically.
     */
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("let*: bad let* in form");
      }

      IEnvironment localEnv = new Environment(env);
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        localEnv.put(var, UNDEFINED);
      }
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        Object init = ((List<Object>) node).get(1);
        localEnv.put(var, evaluator.eval(init, localEnv));
      }

      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      return evaluator.eval(expression.getLast(), localEnv);
    }
  },
  COND("cond") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new IllegalArgumentException("Source expression failed to match any pattern in form (cond)");
      }
      for (int i = 1; i < expression.size(); i++) {
        Object node = expression.get(i);
        if (!(node instanceof List)) {
          throw new IllegalArgumentException("Invalid clause in subform " + node);
        }
        List<Object> subform = (List<Object>) node;
        Object clause = subform.get(0);
        if ("else".equals(clause)) {
          if (i == expression.size() - 1) {
            for (int s = 1; i < subform.size() - 1; i++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
          throw new IllegalArgumentException("cond: else must be the last clause in subform");
        }
        if (SCMBoolean.valueOf(evaluator.eval(clause, env))) {
          for (int s = 1; s < subform.size() - 1; s++) {
            evaluator.eval(subform.get(s), env);
          }
          return evaluator.eval(subform.get(subform.size() - 1), env);
        }
      }
      throw new IllegalArgumentException("Source expression failed to match any pattern in form (cond)");
    }
  },
  CASE("case") {
    private final String ELSE = "else";
    private final Eqv eqv = new Eqv();

    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new IllegalArgumentException("Source expression failed to match any pattern in form (case)");
      }
      Object key = evaluator.eval(expression.get(1), env);
      for (int i = 2; i < expression.size(); i++) {
        Object node = expression.get(i);
        if (!(node instanceof List)) {
          throw new IllegalArgumentException("Invalid clause in subform " + node);
        }
        List<Object> subform = (List<Object>)node;
        Object datum = subform.get(0);
        if (ELSE.equals(datum)) {
          if (i == expression.size() - 1) {
            for (int s = 1; s < subform.size() - 1; s++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
          throw new IllegalArgumentException("cond: else must be the last clause in subform");
        }
        if (!(datum instanceof List)) {
          throw new IllegalArgumentException("Invalid clause in subform " + datum);
        }
        for (Object n : ((List<Object>)datum)) {
          if (eqv.apply(key, n)) {
            for (int s = 1; i < subform.size() - 1; i++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
        }
      }
      throw new IllegalArgumentException("Source expression failed to match any pattern in form (case)");
    }
  },
  AND("and") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Boolean result = Boolean.TRUE;
      if (expression.size() > 1) {
        for (Object arg : expression.subList(1, expression.size())) {
          result = result && SCMBoolean.valueOf(evaluator.eval(arg, env));
          if (!result) {
            return SCMBoolean.toSCMBoolean(result);
          }
        }
      }
      return SCMBoolean.toSCMBoolean(result);
    }
  },
  OR("or") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Boolean result = Boolean.FALSE;
      if (expression.size() > 1) {
        for (Object arg : expression.subList(1, expression.size())) {
          result = result || SCMBoolean.valueOf(evaluator.eval(arg, env));
          if (result) {
            return SCMBoolean.toSCMBoolean(result);
          }
        }
      }
      return SCMBoolean.toSCMBoolean(result);
    }
  },
  BEGIN("begin") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      for (int i = 1; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), env);
      }
      return evaluator.eval(expression.getLast(), env);
    }
  },
  NAMED_LET("named let") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  DELAY("delay") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return new Promise(Collections.emptyList(), expression.get(1));
    }
  };

  private final String syntax;

  private static final Map<String, SpecialForm> SPECIAL_FORMS = new HashMap<String, SpecialForm>(values().length);

  static {
    for (SpecialForm specialForm : values()) {
      SPECIAL_FORMS.put(specialForm.getSyntax(), specialForm);
    }
  }

  public static SpecialForm get(String key) {
    return SPECIAL_FORMS.get(key);
  }

  SpecialForm(String syntax) {
    this.syntax = syntax;
  }

  public String getSyntax() {
    return syntax;
  }

  @Override
  public String toString() {
    return syntax;
  }
}
