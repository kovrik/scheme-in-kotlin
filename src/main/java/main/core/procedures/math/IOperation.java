package main.core.procedures.math;

public interface IOperation<T> {

  T zero();

  T apply(T first, T second);
}
