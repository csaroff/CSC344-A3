sealed abstract class Tree
case class Sum(l: Tree, r: Tree) extends Tree {
  override def toString():String =
  "(" + l.toString() + "+" + r.toString() + ")"
}
case class Var(n: String) extends Tree {
  override def toString() = n
}
case class Const(v: Double) extends Tree {
  override def toString() = v.toString
}
case class Power(x:Var, y:Const) extends Tree {
  override def toString() = x + "^" + y
}
case class Product(x:Tree, y:Tree) extends Tree {
  override def toString() = x + "*" + y
}
object Math {
  type Environment = String => Double
  def eval(t: Tree, env: Environment): Double = t match {
    case Sum(l, r) => eval(l, env) + eval(r, env)
    case Product(l, r) => eval(l, env) * eval(r, env)
    case Power(x, y) => java.lang.Math.pow(eval(x, env), eval(y, env))
    case Var(n) => env(n)
    case Const(v) => v
  }
  def simplify(t: Tree): Tree = t match {
    case Power(l, Const(y)) if (y == 1) => l
    case Product(l,r) if (r==Const(1)) => simplify(l)
    case Product(l,r) if (l==Const(1)) => simplify(r)
    case Product(l,r) => Product(simplify(l), simplify(r))
    case Sum(Const(a), Const(b)) => Const(a + b)
    case Sum(l, r) if (l==r) => Product(Const(2), l)
    case Sum(l, r) => Sum(simplify(l), simplify(r))
    case _ => t
  }
  def derive(t: Tree, v: String): Tree = t match {
    case Power(Var(n), Const(y)) if (v == n) => Product(Const(y),
      Power(Var(n), Const(y - 1)))
    case Sum(l, r) => Sum(derive(l, v), derive(r, v))
    case Var(n) if (v == n) => Const(1)
    case _ => Const(0)
  }
  def main(args: Array[String]):Unit= {
    val exp: Tree = Power(Var("x"), Const(2))
    println("Expression: " + exp + "=" + simplify(exp))
    println("Derivative relative to x:\n " + derive(exp, "x") + " = " +
      simplify(derive(exp, "x")))
    println("Derivative relative to y:\n " + derive(exp, "y") + " = " +
      simplify(derive(exp, "y")))
    val env: Environment = { case "x" => 5 case "y" => 7 }
    println("Evaluation with x=5, y=7: " + eval(exp, env))
  }
}
