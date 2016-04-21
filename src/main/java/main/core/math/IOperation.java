package main.core.math;

public interface IOperation<T> {

  T zero();

  T apply(T first, T second);
}
