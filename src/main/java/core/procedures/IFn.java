package core.procedures;

import java.util.concurrent.Callable;
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
public interface IFn extends Runnable, Callable {

  Object invoke();

  Object invoke(Object arg1);

  Object invoke(Object arg1, Object arg2);

  Object invoke(Object arg1, Object arg2, Object arg3);

  Object invoke(Object arg1, Object arg2, Object arg3, Object arg4);

  Object invoke(Object... args) throws ExecutionException, InterruptedException;
}
