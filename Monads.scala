import scala.language.higherKinds

object Monads {
  type Name = String

  abstract trait Monad[A] {
    type M[A]
    def unitM(a: A) : M[A]
    def bindM[B](ma: M[A], f: A => M[B]) : M[B]
    def showM(ma: M[A]) : String
  }
	
  // TODO - seal?
  abstract class Term
  case class Var(name: Name) extends Term
  case class Lam(arg: Name, body: Term) extends Term
  case class App(f: Term, v: Term) extends Term
  case class Add(f: Term, v: Term) extends Term
  case class Con(v: Int) extends Term

  abstract class Value;

  abstract class MonadicInterpreter { this: Monad[Value] =>
    case class Num(i: Int) extends Value
    case class Fun(f: Value => M[Value]) extends Value
    case object Wrong extends Value

    type Environment = List[(Name, Value)]

    def showval(v: Value) : String = v match {
      case Wrong => "<wrong>"
      case Num(i) => i.toString()
      case Fun(f) => "<function>"
    }
		
    def interp(t: Term, e: Environment) : M[Value] = t match {
      case Var(x) => lookup(x, e)
      case Con(i) => unitM(Num(i))
      case Add(u, v) => bindM(interp(u, e), (a: Value) => bindM(interp(v, e), (b: Value) => add(a, b)))
      case Lam(x, v) => unitM(Fun(a => interp(v, (x, a) :: e)))
      case App(t, u) => bindM(interp(t, e), (f: Value) => bindM(interp(u, e), (a: Value) => app(f, a)))
    }
		
    def lookup(x: Name, e: Environment) : M[Value] = e match {
      case List() => unitM(Wrong)
      case (y, b)::es => if (x == y) unitM(b) else lookup(x, es)
    }
		
    def add(a: Value, b: Value) : M[Value] = (a, b) match {
      case (Num(i), Num(j)) => unitM(Num(i+j))
      case _ => unitM(Wrong)
    }
		
    def app(f: Value, b: Value) : M[Value] = (f, b) match {
      case (Fun(k), v) => k(v)
      case _ => unitM(Wrong)
    }
		
    def test(t: Term) : String = showM(interp(t, List()))
  }
	

  // Variation 0: Standard interpreter
  trait IdentityMonad[A] extends Monad[A] {
    type M[A] = A

    def unitM(a: A) : A = a
    def bindM[B](a: A, f: A => B) : B = f(a)
  }

  class StandardInterpreter extends MonadicInterpreter with IdentityMonad[Value]
  {
    def showM(a: Value) = showval(a)
  }
	

  // Variation 1: Error messages
  abstract class dataE[A]
  case class Success[A](a: A) extends dataE[A]
  case class Error[A](s: String) extends dataE[A]

  trait ErrorMonad[A] extends Monad[A] {
    type M[A] = dataE[A]

    def unitM(a: A) : dataE[A] = Success(a)
    def errorE(s: String) : dataE[A] = Error(s)

    def bindM[B](a: dataE[A], f: A => dataE[B]) : dataE[B] = a match {
      case Success(v) => f(v)
      case Error(s) => Error(s)
    }
  }

  class ErrorInterpreter extends MonadicInterpreter with ErrorMonad[Value]
  {
    override def lookup(x: Name, e: Environment) : M[Value] = e match {
      case List() => Error("unbound variable " + x)
      case (y, b)::es => if (x == y) unitM(b) else lookup(x, es)
    }

    override def add(a: Value, b: Value) : M[Value] = (a, b) match {
      case (Num(i), Num(j)) => unitM(Num(i+j))
      case _ => Error("should be numbers:" + showval(a) + ", " + showval(b))
    }
		
    override def app(f: Value, b: Value) : M[Value] = (f, b) match {
      case (Fun(k), v) => k(v)
      case _ => Error("should be function: " + showval(f))
    }

    def showM(a: dataE[Value]) = a match {
      case Success(v) => "Success: " + showval(v)
      case Error(s) => "Error: " + s
    }
  }


  def main(args: Array[String]) {
    val term0 : Term = App (Lam ("x", Add( (Var("x")), (Var("x")) )/* /Add */ )/* /Lam */, Add( Con(10), Con(11) ) )
    val interpreter0 = new StandardInterpreter()
    val result0 = interpreter0.test(term0)
    println(result0)

    val interpreter1 = new ErrorInterpreter()
    val result1 = interpreter1.test(term0)
    println(result1)

    val term1 = App(Con(1), Con(2))
    val result2 = interpreter1.test(term1)
    println(result2)
  }
}
