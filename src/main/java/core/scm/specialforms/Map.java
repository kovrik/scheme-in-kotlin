package core.scm.specialforms;

import core.environment.Environment;
import core.evaluator.Evaluator;
import core.exceptions.WrongTypeException;
import core.procedures.generic.Count;
import core.procedures.generic.Get;
import core.scm.SCMCons;
import core.scm.SCMSymbol;
import core.scm.SCMThunk;
import core.scm.SCMVector;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/* Syntax:
 * (future <expression>)
 */
public enum Map implements ISpecialForm {
  MAP;

  private final Count count = new Count();
  private final Get get = new Get();

  @Override
  public Object eval(List<Object> expression, Environment env, Evaluator evaluator) {

    /* Check that all lists/vectors are of the same size */
    final int size = count.apply1(expression.get(2));
    for (int i = 2; i < expression.size(); i++) {
      Object coll = expression.get(i);
      /* Check type */
      if (!(coll instanceof SCMVector) && !(coll instanceof Collection) && !(coll instanceof CharSequence)) {
        throw new WrongTypeException(toString(), "List or Vector or Set", coll);
      }
      /* Check size */
      if (count.apply1(coll) != size) {
        throw new IllegalArgumentException("map: all collections must be of the same size");
      }
    }

    List<List> lists = new ArrayList<>(size);
    for (int i = 0; i < size - 1; i++) {
      /* Add procedure as first element */
      lists.add(SCMCons.list(expression.get(i)));
      /* Now add each Nth element of all lists */
      for (int n = 0; n < expression.size() - 1; n++) {
        Object coll = expression.get(n);
        Object list = (coll instanceof Set) ? new ArrayList<>((java.util.Set)coll) : coll;
        Object e = get.apply(list, i);
        if ((e instanceof List) || (e instanceof SCMSymbol)) {
          lists.get(i).add(Quote.quote(e));
        } else {
          lists.get(i).add(e);
        }
      }
    }
    SCMCons<Object> result = SCMCons.list(SCMSymbol.of("list"));
    result.addAll(lists);
    /* Return Thunk that will be evaluated and produce results */
    return new SCMThunk(result);
  }

  @Override
  public String toString() {
    return "map";
  }
}
