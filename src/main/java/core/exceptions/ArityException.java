package core.exceptions;

/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/
public class ArityException extends IllegalArgumentException {

  public ArityException(int actual, String name) {
    this(actual, name, null);
  }

  public ArityException(int actual, int expected, String name) {
    this(actual, expected, name, null);
  }

  public ArityException(int actual, String name, Throwable cause) {
    super("Wrong number of arguments (" + actual + ") passed to: " + name, cause);
  }

  public ArityException(int actual, int expected, String name, Throwable cause) {
    super("Wrong number of arguments (actual: " + actual + ", expected: " + expected + ") passed to: " + name, cause);
  }
}
