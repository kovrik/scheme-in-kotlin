package main.core.specialforms;

import main.core.ast.SCMList;
import main.core.evaluator.IEvaluator;
import main.core.math.bool.Eqv;
import main.core.procedures.Procedure;
import main.environment.Environment;
import main.environment.IEnvironment;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum SpecialForm implements ISpecialForm {

  /* Fundamental forms */
  DEFINE("define") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      // TODO define procedure syntax
      env.put(expression.get(1), evaluator.eval(expression.get(2), env));
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
      if ((Boolean) evaluator.eval(test, env)) {
        return evaluator.eval(consequence, env);
      } else {
        return evaluator.eval(alternative, env);
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
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("let: bad let in form");
      }

      IEnvironment localEnv = new Environment(env);
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        Object init = ((List<Object>) node).get(1);
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
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
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
        if ((Boolean) evaluator.eval(clause, env)) {
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
          result = result && (Boolean)(evaluator.eval(arg, env));
          if (!result) {
            return result;
          }
        }
      }
      return result;
    }
  },
  OR("or") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Boolean result = Boolean.FALSE;
      if (expression.size() > 1) {
        for (Object arg : expression.subList(1, expression.size())) {
          result = result || (Boolean)(evaluator.eval(arg, env));
          if (result) {
            return true;
          }
        }
      }
      return result;
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
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
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
