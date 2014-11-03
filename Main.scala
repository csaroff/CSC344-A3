import scala.collection.JavaConversions._;
import scala.collection.mutable.HashMap;
sealed abstract class Tree
case class Const(v: Int) extends Tree {
  override def toString() = v.toString;
}
case class Var(n: String) extends Tree {
  override def toString() = n;
}
case class Sum(l: Tree, r: Tree) extends Tree {
  override def toString():String =
  "(" + l.toString() + "+" + r.toString() + ")";
}
case class Difference(l:Tree, r:Tree) extends Tree {
  override def toString():String =
  "(" + l.toString() + "-" + r.toString() + ")";
}
case class Product(l:Tree, r:Tree) extends Tree {
  override def toString():String =
  "(" + l.toString() + "*" + r.toString() + ")";
}
case class Quotient(l:Tree, r:Tree) extends Tree{
  override def toString():String =
  "(" + l.toString() + "/" + r.toString() + ")";
}
object Main {
  type BindingList = String => Option[Int];
  def substituteBindings (bl: BindingList, t: Tree):Tree = t match{
    case Const(v) => Const(v);
    case Var(n) => 
      if(bl(n).isDefined){
        Const(bl(n).get);
      }else{
        Var(n);
      }
    //Const(bl(n));
    case Sum(l, r) => Sum(substituteBindings(bl, l), substituteBindings(bl, r));
    case Difference(l, r) => Difference(substituteBindings(bl, l), substituteBindings(bl, r));
    case Product(l, r) => Product(substituteBindings(bl, l), substituteBindings(bl, r));
    case Quotient(l, r) => Quotient(substituteBindings(bl, l), substituteBindings(bl, r));
  }
  def simplify(t:Tree):Tree = t match {
    case Var(x) => Var(x);
    case Const(x) => Const(x);
    case Sum(l,r) => simplifyTriple(Sum(simplify(l),simplify(r)));
    case Product(l,r) => simplifyTriple(Product(simplify(l),simplify(r)));
    case Difference(l,r) => simplifyTriple(Difference(simplify(l),simplify(r)));
    case Quotient(l,r) => simplifyTriple(Quotient(simplify(l),simplify(r)));
  }
  def simplifyTriple(t: Tree): Tree = t match {
    case Sum(Const(l), Const(r)) => Const(l + r);
    case Sum(l, Const(r)) if(r==0) => simplify(l);
    case Sum(Const(l), r) if(l==0) => simplify(r);
    case Sum(l, Difference(Const(a), b)) if(a==0 && l==b) => Const(0);
    case Sum(l, r) 
      if(simplify(l).isInstanceOf[Const] && simplify(r).isInstanceOf[Const]) 
        => simplify(Sum(simplify(l), simplify(r)));
    case Sum(l,r) => Sum(simplify(l), simplify(r));
  //case Sum(l, r) if (l==r) => Product(Const(2), simplify(l));

    case Difference(Const(l), Const(r)) => Const(l-r);
    case Difference(l, Const(r)) if(r==0) => simplify(l);
    case Difference(l, r) if(l==r) => Const(0);
    case Difference(l, r) 
      if(simplify(l).isInstanceOf[Const] && simplify(r).isInstanceOf[Const])
        => simplify(Difference(simplify(l), simplify(r)));
    case Difference(l, r) => Difference(simplify(l), simplify(r));

    case Product(Const(l), Const(r)) => Const(l*r);
    case Product(l,Const(r)) if (r==0) => Const(0);
    case Product(Const(l), r) if (l==0) => Const(0);
    case Product(l,Const(r)) if (r==1) => simplify(l);
    case Product(l,r) if (l==Const(1)) => simplify(r);
    case Product(l,r)
      if(simplify(l).isInstanceOf[Const]&&simplify(r).isInstanceOf[Const])
        => simplify(Product(simplify(l), simplify(r)));
    case Product(l,r) => Product(simplify(l), simplify(r));

    case Quotient(Const(l), Const(r)) => Const(l/r);
    case Quotient(Const(l), r) if(l==0) => Const(0);
    case Quotient(l, Const(r)) if(r==1) => simplify(l);
    case Quotient(l, r) if(l==r) => Const(1);
    case Quotient(l,r) 
      if(simplify(l).isInstanceOf[Const]&&simplify(r).isInstanceOf[Const])
        => simplify(Quotient(simplify(l), simplify(r)));
    case Quotient(l,r) => Quotient(simplify(l), simplify(r));

    case _ => t;
  }
  def main(args: Array[String]):Unit= {
    var expressions: List[Tree] = List(
     Sum(Var("x"), Const(0)),
     Sum(Const(0), Var("x")),
     Difference(Var("x"), Const(0)),
     Difference(Var("x"), Var("x")),
     Sum(Var("x"), Difference(Const(0), Var("x"))),
     Product(Var("x"), Const(0)),
     Product(Const(0), Var("x")),
     Product(Var("x"), Const(1)),
     Product(Const(1), Var("x")),
     Quotient(Const(0), Var("x")),
     Quotient(Var("x"), Const(1)),
     Quotient(Var("x"), Var("x")),
     Sum(Var("x"), Product(Var("x"), Difference(Var("y"), Quotient(Var("z"), Const(2))))),
     Sum(Difference(Var("z"), Const(2)), Product(Var("x"), Const(5))),
     Sum(Const(1), Var("a")),
     Product(Sum(Var("a"), Var("b")), Difference(Var("a"), Const(1))),
     Product(Sum(Var("x"), Var("y")), Quotient(Sum(Const(3), Var("x")), Const(4))),
     Sum(Difference(Const(1), Var("a")), Const(3))
    )

    for(expression <- expressions){
      val varToConst = new HashMap[String, Int]();
      System.out.println("Expression: " + expression);
      System.out.println("Please enter the binding list: ");
      val sc = new java.util.Scanner(scala.io.StdIn.readLine());
      while(sc.hasNext()){
        varToConst.put(sc.next(), sc.nextInt());
      }
      for((key,value) <- varToConst){
        System.out.println("<" + key + ", " + value + ">");
      }
      val bindingList : BindingList = {case x => varToConst.get(x);}
      //http://stackoverflow.com/questions/5740906/how-to-check-for-null-in-a-single-statement-in-scala
      val exp = substituteBindings(bindingList, expression);
      println("Expression: " + exp + "=" + simplify(exp));
    }

  }
}
