package core.procedures;

import core.exceptions.ArityException;
import java.util.concurrent.ExecutionException;

/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/
public abstract class AFn implements IFn {

  @Override
  public Object invoke() {
    return throwArity(0);
  }

  @Override
  public Object invoke(Object arg1) {
    return throwArity(1);
  }

  @Override
  public Object invoke(Object arg1, Object arg2) {
    return throwArity(2);
  }

  @Override
  public Object invoke(Object arg1, Object arg2, Object arg3) {
    return throwArity(3);
  }

  @Override
  public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
    return throwArity(4);
  }

  @Override
  public Object invoke(Object... args) throws ExecutionException, InterruptedException {
    return throwArity(args.length);
  }

  @Override
  public Object call() throws Exception {
    return invoke();
  }

  @Override
  public void run() {
    invoke();
  }

  public Object throwArity(int n) {
    throw new ArityException(n, getName());
  }

  public String getName() {
    return getClass().getSimpleName();
  }

  @Override
  public String toString() {
    return "#<procedure " + getName() + ">";
  }
}
