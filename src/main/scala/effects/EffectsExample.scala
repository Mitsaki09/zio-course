package effects

object EffectsExample {
  /**
  Covariance and Contravariance
  In Scala, you can control how types behave in inheritance. The + symbol before a type parameter (+E, +A) indicates covariance.

  Covariance (+):
  If a type A is covariant (denoted as +A), it means that if X is a subtype of Y (e.g., Cat is a subtype of Animal), then MyEither[E, X] will also be a subtype of MyEither[E, Y].

  Contravariance (-):
  If there were a - instead of +, it would indicate contravariance. In contravariant types, the relationship works in the opposite direction.
   */

  sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B]

    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B]
  }

  case class MyLeft[+E](value: E) extends MyEither[E, Nothing] {
    def map[B](f: Nothing => B): MyEither[E, B] = this

    def flatMap[EE >: E, B](f: Nothing => MyEither[EE, B]): MyEither[EE, B] = this
  }

  case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
    def map[B](f: A => B): MyEither[Nothing, B] = MyRight(f(value))

    def flatMap[EE >: Nothing, B](f: A => MyEither[EE, B]): MyEither[EE, B] = f(value)
  }

  def processNumber(n: Int): MyEither[String, Int] = {
    if (n >= 0) MyRight(n * n)
    else MyLeft("Negative numbers are not allowed")
  }

  def main(args: Array[String]): Unit = {
    val number = 5

    val result: MyEither[String, Int] = processNumber(number)
      .flatMap(n => MyRight(n + 10))
      .map(_ * 2)

    result match {
      case MyRight(value) => println(s"Success: $value")
      case MyLeft(error) => println(s"Error: $error")
    }
  }

}
