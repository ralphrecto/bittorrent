/**
 * Created by ralphrecto on 1/17/16.
 */
object Util {
  implicit class FunctionPipe[A](underlying : A) {
    def |>[B](f: A => B) : B = f(underlying)
  }
}
