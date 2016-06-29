package core.scm.specialforms;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.procedures.delayed.SCMPromise;
import core.procedures.equivalence.Eqv;
import core.scm.SCMBoolean;
import core.scm.SCMList;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.errors.SCMError;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum SCMSpecialForm implements ISpecialForm {

  UNSPECIFIED("#<unspecified>") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return UNSPECIFIED;
    }
  },
  DOT("DOT") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return DOT;
    }
  },
  /* Fundamental forms */
  DEFINE("define") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object definition = expression.get(1);
      if (definition instanceof SCMSymbol) {
        Object body = expression.get(2);
        env.put(definition, evaluator.eval(body, env));
      } else if (definition instanceof SCMList) {
        // procedure
        // implicit `begin`
        SCMList<Object> body = new SCMList<Object>(BEGIN);
        body.addAll(expression.subList(2, expression.size()));
        if (((SCMList) definition).isEmpty()) {
          throw new IllegalArgumentException("lambda: bad lambda in form: " + expression);
        }
        SCMSymbol name = (SCMSymbol)((SCMList)definition).pop();
        // TODO Optimize DOT removal
        env.put(name, new SCMProcedure(name, (SCMList<SCMSymbol>) definition, body, env, ((SCMList)definition).remove(DOT)));
      } else {
        throw new IllegalArgumentException("define: bad `define` in form: " + expression);
      }
      return UNSPECIFIED;
    }
  },
  LAMBDA("lambda") {
    public SCMProcedure eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("lambda: bad lambda in form: " + expression);
      }
      Object args = expression.get(1);

      // implicit `begin`
      SCMList<Object> body = new SCMList<Object>(BEGIN);
      body.addAll(expression.subList(2, expression.size()));
      if (args instanceof List) {
        return new SCMProcedure("", (List<SCMSymbol>)args, body, env);
      } else {
        /* Variadic arity */
        return new SCMProcedure("", new SCMList<SCMSymbol>((SCMSymbol) expression.get(1)), body, env, true);
      }
    }
  },
  IF("if") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object test = expression.get(1);
      Object consequence = expression.get(2);
      if (SCMBoolean.valueOf(evaluator.eval(test, env))) {
        return evaluator.eval(consequence, env);
      } else {
        if (expression.size() < 4) {
          return UNSPECIFIED;
        }
        Object alternative = expression.get(3);
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
      if ((expression.get(1) instanceof SCMList) && (((SCMList)expression.get(1)).isEmpty())) {
        return SCMList.EMPTY_LIST;
      }
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
      env.findAndPut(expression.get(1), evaluator.eval(expression.get(2), env));
      return UNSPECIFIED;
    }
  },
  /* Library forms */
  DO("do") {
    // TODO
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LET("let") {

    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {

      if (expression.size() < 3) {
        throw new IllegalArgumentException("let: bad let in form: " + expression);
      }

      if (expression.get(1) instanceof List) {
        IEnvironment localEnv = new Environment(env);
        for (Object binding : ((List)expression.get(1))) {
          Object var = ((List)binding).get(0);
          Object init = ((List)binding).get(1);
          if (localEnv.get(var) != null) {
            throw new IllegalArgumentException("let: duplicate bound variable " + var);
          }
          localEnv.put(var, evaluator.eval(init, env));
        }

        for (int i = 2; i < expression.size() - 1; i++) {
          evaluator.eval(expression.get(i), localEnv);
        }
        return evaluator.eval(expression.getLast(), localEnv);

      } else if (expression.get(1) instanceof SCMSymbol) {
        // TODO Optimize and cleanup
        /* Named let via letrec */
        Object o = expression.get(1);
        if (!(o instanceof SCMSymbol)) {
          throw new IllegalArgumentException("let: bad let in form: " + expression);
        }
        // lambda
        SCMList<Object> lambdaArgs = new SCMList<Object>();
        SCMList<Object> initValues = new SCMList<Object>();
        for (Object binding : (List<SCMSymbol>)expression.get(2)) {
          Object arg = ((List)binding).get(0);
          if (lambdaArgs.contains(arg)) {
            throw new IllegalArgumentException("let: duplicate bound variable: " + arg);
          }
          lambdaArgs.add(arg);
          initValues.add(((List)binding).get(1));
        }
        SCMList<Object> lambdaBody = (SCMList)expression.get(3);
        SCMList<Object> lambda = new SCMList<Object>(LAMBDA, lambdaArgs, lambdaBody);
        SCMSymbol name = (SCMSymbol)o;
        SCMList<Object> l = new SCMList<Object>();
        l.add(new SCMList<Object>(name, lambda));

        SCMList<Object> body = new SCMList<Object>(name);
        body.addAll(initValues);

        SCMList<Object> letrec = new SCMList<Object>(LETREC);
        letrec.add(l);
        letrec.add(body);
        return LETREC.eval(letrec, new Environment(env), evaluator);
      }
      throw new IllegalArgumentException("let: bad let in form: " + expression);
    }
  },
  LET_S("let*") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalArgumentException("let*: bad let* in form: " + expression);
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
        throw new IllegalArgumentException("letrec: bad letrec in form: " + expression);
      }

      IEnvironment localEnv = new Environment(env);
      for (Object node : ((List<Object>) expression.get(1))) {
        Object var = ((List<Object>) node).get(0);
        localEnv.put(var, UNSPECIFIED);
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
    private final SCMSymbol ELSE = new SCMSymbol("else");

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
        if (ELSE.equals(clause)) {
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
    private final SCMSymbol ELSE = new SCMSymbol("else");
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
          throw new IllegalArgumentException("case: else must be the last clause in subform");
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
      return null;
    }
  },
  AND("and") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object eval = SCMBoolean.TRUE;
      if (expression.size() > 1) {
        for (Object arg : expression.subList(1, expression.size())) {
          eval = evaluator.eval(arg, env);
          if (!SCMBoolean.valueOf(eval)) {
            return eval;
          }
        }
      }
      return eval;
    }
  },
  OR("or") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      Object eval = SCMBoolean.FALSE;
      if (expression.size() > 1) {
        for (Object arg : expression.subList(1, expression.size())) {
          eval = evaluator.eval(arg, env);
          if (SCMBoolean.valueOf(eval)) {
            return eval;
          }
        }
      }
      return eval;
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
  DELAY("delay") {
    public SCMPromise eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      return new SCMPromise(Collections.<SCMSymbol>emptyList(), expression.get(1));
    }
  },
  CLASSOF("class-of") {
    public String eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() > 2) {
        throw new ArityException(expression.size() - 1, "class-of");
      }
      return evaluator.eval(expression.get(1), env).getClass().getName();
    }
  },
  ERROR("error") {
    public Object eval(SCMList<Object> expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() > 2) {
        throw new ArityException(expression.size() - 1, "error");
      }
      throw new SCMError(evaluator.eval(expression.get(1), env).toString());
    }
  }
  ;

  private final String syntax;

  private static final Map<String, SCMSpecialForm> SPECIAL_FORMS = new HashMap<String, SCMSpecialForm>(values().length);

  static {
    for (SCMSpecialForm specialForm : values()) {
      SPECIAL_FORMS.put(specialForm.getSyntax(), specialForm);
    }
  }

  public static SCMSpecialForm get(String key) {
    return SPECIAL_FORMS.get(key);
  }

  SCMSpecialForm(String syntax) {
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
