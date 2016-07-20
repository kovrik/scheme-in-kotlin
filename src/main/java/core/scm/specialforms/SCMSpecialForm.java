package core.scm.specialforms;

import core.environment.Environment;
import core.environment.IEnvironment;
import core.evaluator.IEvaluator;
import core.exceptions.ArityException;
import core.exceptions.IllegalSyntaxException;
import core.procedures.delayed.SCMPromise;
import core.procedures.equivalence.Eqv;
import core.scm.SCMBoolean;
import core.scm.SCMCons;
import core.scm.SCMProcedure;
import core.scm.SCMSymbol;
import core.scm.errors.SCMError;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public enum SCMSpecialForm implements ISpecialForm {

  UNSPECIFIED("#<unspecified>") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      return UNSPECIFIED;
    }
  },
  DOT(".") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      return DOT;
    }
  },
  /* Fundamental forms */
  // TODO Check that internal definitions are at the beginning only!
  DEFINE("define") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      Object definition = expression.get(1);
      if (definition instanceof SCMSymbol) {
        /* Normal definition */
        Object body = expression.get(2);
        env.put(definition, evaluator.eval(body, env));
      } else if (definition instanceof SCMCons) {
        /* Function shorthand definition
         * expression = (define (name a1 a2 ... an [. ar]) f1 f2 ... fn)
         *              |   0   | 1 definition           | 3 body      |
         */
        if (((SCMCons) definition).isEmpty()) {
          throw new IllegalSyntaxException("lambda: bad lambda in form: " + expression);
        }
        /* Add implicit `begin` */
        SCMCons<Object> body = SCMCons.list(BEGIN);
        /* Add all body forms */
        body.addAll(expression.subList(2, expression.size()));

        /* Get name and remove it */
        SCMSymbol name = (SCMSymbol)((SCMCons)definition).pop();
        /* Everything that remains should be a list of params */
        /* Check if it is a cons, not a list (hence, we have a dotted notation) */
        if (((SCMCons)definition).isList()) {
          /* No varargs */
          env.put(name, new SCMProcedure(name, (SCMCons<SCMSymbol>) definition, body, env, false));
        } else {
          /* Varargs */
          /* Flatten chain of conses into a list of params */
          List<SCMSymbol> params = SCMCons.flatten((List<SCMSymbol>)definition);
          env.put(name, new SCMProcedure(name, params, body, env, true));
        }
      } else {
        throw new IllegalSyntaxException("define: bad `define` in form: " + expression);
      }
      return UNSPECIFIED;
    }
  },
  LAMBDA("lambda") {
    @Override
    public SCMProcedure eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalSyntaxException("lambda: bad lambda in form: " + expression);
      }
      Object args = expression.get(1);

      // implicit `begin`
      SCMCons<Object> body = SCMCons.list(BEGIN);
      body.addAll(expression.subList(2, expression.size()));
      /* Check if args is a proper list or a pair (cons) */
      if (args instanceof List) {
        return new SCMProcedure("", (List<SCMSymbol>)args, body, env);
      } else {
        /* Variadic arity */
        return new SCMProcedure("", SCMCons.list((SCMSymbol)expression.get(1)), body, env, true);
      }
    }
  },
  IF("if") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
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
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new ArityException(0, getSyntax());
      }
      Object test = expression.get(1);
      if (!SCMBoolean.valueOf(evaluator.eval(test, env))) {
        return UNSPECIFIED;
      } else {
        if (expression.size() > 1) {
          Object result = null;
          for (int i = 2; i < expression.size(); i++) {
            result = evaluator.eval(expression.get(i), env);
          }
          return result;
        }
        return UNSPECIFIED;
      }
    }
  },
  QUOTE("quote") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      Object exp = expression.get(1);
      if ((exp instanceof SCMCons) && (((SCMCons)exp).isEmpty())) {
        return SCMCons.NIL;
      }
      /* Numerical constants, string constants, character constants, and boolean constants
       * evaluate "to themselves"; they need not be quoted.
       * Numbers, strings and chars are "evaluated" by Reader */
      if (SCMBoolean.TRUE.equals(exp)) {
        return SCMBoolean.TRUE;
      }
      if (SCMBoolean.FALSE.equals(exp)) {
        return SCMBoolean.FALSE;
      }
      return exp;
    }
  },
  UNQUOTE("unquote") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  UNQUOTE_SPLICING("unquote-splicing") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  QUASIQUOTE("quasiquote") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      return expression;
    }
  },
  DEFINE_SYNTAX("define-syntax") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LET_SYNTAX("let-syntax") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  LETREC_SYNTAX("letrec-syntax") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  SYNTAX_RULES("syntax-rules") {
    // TODO
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      throw new UnsupportedOperationException("NOT IMPLEMENTED!");
    }
  },
  SET("set!") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      env.findAndPut(expression.get(1), evaluator.eval(expression.get(2), env));
      return UNSPECIFIED;
    }
  },
  /* Library forms */
  DO("do") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      /* Syntax:     do <bindings> <clause> <body>
       * <bindings>: ((<variable 1> <init 1> <step 1>) ...),
       * <clause>:   (<test> <expression> ...),
       **/
      if (expression.size() < 3) {
        throw new IllegalSyntaxException("do: bad syntax");
      }

      Object bs = expression.get(1);
      if (!(bs instanceof List)) {
        throw new IllegalSyntaxException("do: bad syntax");
      }
      IEnvironment tempEnv = new Environment(env);
      List bindings = (List) bs;
      List<SCMCons> steps = SCMCons.list();
      /* Init bindings */
      for (Object binding : bindings) {
        if (!(binding instanceof List)) {
          throw new IllegalSyntaxException("do: bad syntax");
        }
        /* Check that init value exists */
        if (((List)binding).size() < 2) {
          throw new IllegalSyntaxException("do: bad syntax");
        }
        Object variable = ((List) binding).get(0);
        Object init = ((List) binding).get(1);
        Object step = null;
        if (((List) binding).size() == 3) {
          step = ((List) binding).get(2);
          /* Put pair of Var and Step */
          steps.add(SCMCons.cons(variable, step));
        }
        /* Check that we have no duplicates among variables */
        if (!tempEnv.containsKey(variable)) {
          tempEnv.put(variable, evaluator.eval(init, tempEnv));
        } else {
          throw new IllegalSyntaxException("let: duplicate identifier: " + variable);
        }
      }

      Object cl = expression.get(2);
      if (!(cl instanceof List)) {
        throw new IllegalSyntaxException("do: bad syntax");
      }
      List clause = (List)cl;
      if (clause.isEmpty()) {
        throw new IllegalSyntaxException("do: bad syntax");
      }
      Object test = clause.get(0);

      List body = expression.subList(3, expression.size());
      /* While test evaluates to #f */
      while (!SCMBoolean.valueOf(evaluator.eval(test, tempEnv))) {
        /* Evaluate command expressions */
        for (Object exp : body) {
          /* Evaluate each expression */
          evaluator.eval(exp, tempEnv);
        }
        /* Evaluate steps */
        Map<Object, Object> freshLocations = new HashMap<>(steps.size());
        for (SCMCons step : steps) {
          Object variable = step.car();
          Object s = step.cdr();
          freshLocations.put(variable, evaluator.eval(s, tempEnv));
        }
        /* Now store results */
        for (Map.Entry<Object, Object> entry : freshLocations.entrySet()) {
          tempEnv.put(entry.getKey(), entry.getValue());
        }
      }
      /* Test evaluated to #f */
      List expressions = clause.subList(1, clause.size());
      Object value = UNSPECIFIED;
      for (Object exp : expressions) {
        /* Evaluate each expression */
        value = evaluator.eval(exp, tempEnv);
      }
      /* Return value of last expression or UNSPECIFIED */
      return value;
    }
  },
  LET("let") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalSyntaxException("let: bad let in form: " + expression);
      }

      if (expression.get(1) instanceof List) {
        IEnvironment localEnv = new Environment(env);
        List bindings = (List) expression.get(1);
        for (Object binding : bindings) {
          Object var  = ((List)binding).get(0);
          Object init = ((List)binding).get(1);
          if (localEnv.get(var) != null) {
            throw new IllegalSyntaxException("let: duplicate identifier: " + var);
          }
          localEnv.put(var, evaluator.eval(init, env));
        }

        for (int i = 2; i < expression.size() - 1; i++) {
          evaluator.eval(expression.get(i), localEnv);
        }
        return evaluator.eval(expression.get(expression.size() - 1), localEnv);

      } else if (expression.get(1) instanceof SCMSymbol) {
        // TODO Optimize and cleanup
        /* Named let via letrec */
        Object o = expression.get(1);
        if (!(o instanceof SCMSymbol)) {
          throw new IllegalSyntaxException("let: bad let in form: " + expression);
        }
        // lambda
        SCMCons<Object> lambdaArgs = SCMCons.list();
        SCMCons<Object> initValues = SCMCons.list();
        List bindings = (List)expression.get(2);
        for (Object binding : bindings) {
          Object arg = ((List)binding).get(0);
          if (lambdaArgs.contains(arg)) {
            throw new IllegalSyntaxException("let: duplicate identifier: " + arg);
          }
          lambdaArgs.add(arg);
          initValues.add(((List)binding).get(1));
        }
        Object lambdaBody = expression.get(3);
        SCMCons lambda = SCMCons.list(LAMBDA, lambdaArgs, lambdaBody);
        SCMSymbol name = (SCMSymbol)o;
        SCMCons<SCMCons> l = SCMCons.list();
        l.add(SCMCons.list(name, lambda));

        SCMCons<Object> body = SCMCons.list(name);
        body.addAll(initValues);

        SCMCons<Object> letrec = SCMCons.list(LETREC);
        letrec.add(l);
        letrec.add(body);
        return LETREC.eval(letrec, new Environment(env), evaluator);
      }
      throw new IllegalSyntaxException("let: bad let in form: " + expression);
    }
  },
  LET_S("let*") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalSyntaxException("let*: bad let* in form: " + expression);
      }
      IEnvironment localEnv = new Environment(env);
      List bindings = (List)expression.get(1);
      for (Object binding : bindings) {
        Object var  = ((List)binding).get(0);
        Object init = ((List)binding).get(1);
        localEnv.put(var, evaluator.eval(init, localEnv));
      }

      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      return evaluator.eval(expression.get(expression.size() - 1), localEnv);
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
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 3) {
        throw new IllegalSyntaxException("letrec: bad letrec in form: " + expression);
      }
      IEnvironment localEnv = new Environment(env);
      List bindings = (List)expression.get(1);
//      for (Object binding : bindings) {
//        Object var = ((List)binding).get(0);
//        localEnv.put(var, UNSPECIFIED);
//      }
      for (Object binding : bindings) {
        Object var  = ((List)binding).get(0);
        Object init = ((List)binding).get(1);
        localEnv.put(var, evaluator.eval(init, localEnv));
      }
      for (int i = 2; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), localEnv);
      }
      return evaluator.eval(expression.get(expression.size() - 1), localEnv);
    }
  },
  COND("cond") {
    private final SCMSymbol ELSE = new SCMSymbol("else");

    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new IllegalSyntaxException("Source expression failed to match any pattern in form (cond)");
      }
      for (int i = 1; i < expression.size(); i++) {
        Object node = expression.get(i);
        if (!(node instanceof List)) {
          throw new IllegalSyntaxException("Invalid clause in subform " + node);
        }
        List<Object> subform = (List)node;
        Object clause = subform.get(0);
        if (ELSE.equals(clause)) {
          if (i == expression.size() - 1) {
            for (int s = 1; i < subform.size() - 1; i++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
          throw new IllegalSyntaxException("cond: else must be the last clause in subform");
        }
        if (SCMBoolean.valueOf(evaluator.eval(clause, env))) {
          for (int s = 1; s < subform.size() - 1; s++) {
            evaluator.eval(subform.get(s), env);
          }
          return evaluator.eval(subform.get(subform.size() - 1), env);
        }
      }
      throw new IllegalSyntaxException("Source expression failed to match any pattern in form (cond)");
    }
  },
  CASE("case") {
    private final SCMSymbol ELSE = new SCMSymbol("else");
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() <= 1) {
        throw new IllegalSyntaxException("Source expression failed to match any pattern in form (case)");
      }
      Object key = evaluator.eval(expression.get(1), env);
      for (int i = 2; i < expression.size(); i++) {
        Object node = expression.get(i);
        if (!(node instanceof List)) {
          throw new IllegalSyntaxException("Invalid clause in subform " + node);
        }
        List<Object> subform = (List)node;
        Object datum = subform.get(0);
        if (ELSE.equals(datum)) {
          if (i == expression.size() - 1) {
            for (int s = 1; s < subform.size() - 1; s++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
          throw new IllegalSyntaxException("case: else must be the last clause in subform");
        }
        if (!(datum instanceof List)) {
          throw new IllegalSyntaxException("Invalid clause in subform " + datum);
        }
        for (Object n : ((List)datum)) {
          if (Eqv.eqv(key, n)) {
            for (int s = 1; i < subform.size() - 1; i++) {
              evaluator.eval(subform.get(s), env);
            }
            return evaluator.eval(subform.get(subform.size() - 1), env);
          }
        }
      }
      return UNSPECIFIED;
    }
  },
  AND("and") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      Object result = SCMBoolean.TRUE;
      if (expression.size() > 1) {
        for (int i = 1; i < expression.size(); i++) {
          result = evaluator.eval(expression.get(i), env);
          if (!SCMBoolean.valueOf(result)) {
            return result;
          }
        }
      }
      return result;
    }
  },
  OR("or") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
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
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      for (int i = 1; i < expression.size() - 1; i++) {
        evaluator.eval(expression.get(i), env);
      }
      return evaluator.eval(expression.get(expression.size() - 1), env);
    }
  },
  DELAY("delay") {
    @Override
    public SCMPromise eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() < 2) {
        throw new IllegalSyntaxException("delay: bad `delay` in form: " + expression);
      }
      return new SCMPromise(Collections.emptyList(), expression.get(1));
    }
  },
  ERROR("error") {
    @Override
    public Object eval(List expression, IEnvironment env, IEvaluator evaluator) {
      if (expression.size() > 2) {
        throw new ArityException(expression.size() - 1, "error");
      }
      throw new SCMError(evaluator.eval(expression.get(1), env).toString());
    }
  };

  private final String syntax;

  private static final Map<String, SCMSpecialForm> SPECIAL_FORMS = new HashMap<>(values().length);
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

  protected String getSyntax() {
    return syntax;
  }

  @Override
  public String toString() {
    return syntax;
  }
}
