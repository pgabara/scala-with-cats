package com.github.bhop.cats.monad

object PostOrderCalculatorWithStateMonad {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def eval(input: String): Int =
    evalAll(input.split(" ").toList).runA(List.empty).value

  def evalAll(input: List[String]): CalcState[Int] = {
    import cats.syntax.applicative._
    input.foldLeft[CalcState[Int]](0.pure[CalcState]) { (state, in) => state.flatMap(_ => evalOne(in)) }
  }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  private def operator(fn: (Int, Int) => Int): CalcState[Int] =
    State {
      case a :: b :: tail => (fn(b, a) :: tail, fn(b, a))
      case _ => throw new IllegalStateException("Number of operands is not enough")
    }

  private def operand(n: Int): CalcState[Int] =
    State(state => (n :: state, n))
}
