package gn.cats.tutorials.ch3.datamanipulation

import scala.annotation.tailrec

// 3
object Evaluations {

  /*
    Cats makes the distinction between:
      - evaluating an expression eagerly
      - evaluating lazily and every time you request it
      - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    234565
  }
  // => It's an eager evaluation and the println statement it will be shown in the console even when it's not called in
  // the main function, and if it's called twice, it will only return the result without showing the println statement

  val redoEval: Eval[Int] = Eval.always {
    println("Computing again")
    2365
  }
  // => It's a lazy evaluation that won't be evaluated unless it's called in the main function. If we call the '.value'
  // in main without 'println', it will show the println statement. If it's called twice, it will re-evaluate (println
  // statement) and return the result

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later")
    2315
  }
  // => It's like the 'redoEval', but if the call the evaluation twice, it will only return the result without evaluating
  // again, so the println statement won't be shown again

  val composedEval: Eval[Int] = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))
  val anotherComposedEval: Eval[Int] = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical to "composedEval"

  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute: Eval[Int] = redoEval.memoize
  // will the computed internal value, so it won't recomputed again == remembering a computed value
  // 'memoize' can help to create a chain of evaluations that can be recomputed and memoized at will

  val tutorial: Eval[String] = Eval.always { println("Step 1"); "put the guitar on your lap" }.map { step1 =>
    println("Step 2"); s"'$step1' then put your left hand on the neck"
  }.memoize // remember the value
    .map { steps12 => println("Step 3, more complicated"); s"'$steps12' then with the right hand strike the strings" }
  // Steps 1 and 2 will be printed once, and step 3 will be printed twice

  // implementing defer such that defer(Eval.now) does not run side effects (running println)
  // delaying the evaluation, until calling '.value', regardless if defer is implemented with 'now', 'always' or 'later'
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  // Rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

//  def reverseEval[T](list: List[T]): Eval[List[T]] =
//    if (list.isEmpty) Eval.now(list)
//    else reverseEval(list.tail).map(_ :+ list.head)
  // Not stack safe
  // This function is stack recursive, so if we pass a big list, there is a risk of getting a stack overflow as a result.
  // That's why we need to add the 'defer' method as a wrapper on 'reverseEval'
  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).map(_ :+ list.head))
    // stack safe
  }

  // To ensure a stack safe computation, we can use as well 'loop' and 'advance' methods provided by Eval

  def main(args: Array[String]): Unit = {
//    println(instantEval.value)
//    println(instantEval.value)
//    println(redoEval.value)
//    println(redoEval.value)
//    println(delayedEval.value)
//    println(delayedEval.value)

//    println(composedEval.value)
//    println(composedEval.value)
//    println(anotherComposedEval.value)
//    println(anotherComposedEval.value)

//    println(evalEx1.value)
//    println(evalEx1.value)
    /*
      Computing now
      Computing later
      Computing again
      Computing again
      241610
      Computing again
      Computing again
      241610
     */

//    println(dontRecompute.value)
//    println(dontRecompute.value)

//    println(tutorial.value)
//    println(tutorial.value)

//    defer(Eval.now { println("Now!"); 12 })
//    defer(Eval.now { println("Now!"); 12 }).value
//    println(defer(Eval.now {
//      println("Now!"); 12
//    }).value)

    println(reverseList((0 to 1000).toList))
    println(reverseEval((0 to 10000).toList).value)
  }
}
