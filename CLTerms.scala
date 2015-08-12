package object cl {
  sealed trait Term {
    def *(that: Term) = cl.*(this, that)

    // Given a term M and variable x, M.extract(x) returns a term
    // without x, M', s.t. (M' * x) evaluates the same way as M
    //def extract(x: Var): Term = this match {
    //  case a: Atom if a != x => K * a
    //  case a: Atom if a == x => I
    //  case l * r => S * l.extract(x) * r.extract(x)
    //}
    def extract(x: Var): Term = extractC(x)

    // Implements Algorithm (C)
    // @see http://www.cantab.net/users/antoni.diller/brackets/intro.html
    def extractC(x: Var): Term = this match {
      case (e : Term) if !e.contains(x) => K * e
      case (y : Var) if y == x => I
      case (e : Term) * (y : Var) if !e.contains(x) && y == x => e
      case (e : Term) * (f : Term) * (g : Term) if !e.contains(x) && !f.contains(x) && g.contains(x) =>
        Bp * e * f * g.extractC(x)
      case (e : Term) * (g : Term) * (f : Term) if !e.contains(x) && !f.contains(x) && g.contains(x) =>
        Cp * e * g.extractC(x) * f
      case (e : Term) * (g : Term) * (h : Term) if !e.contains(x) && g.contains(x) && h.contains(x) =>
        Sp * e * g.extractC(x) * h.extractC(x)
      case (e : Term) * (g : Term) if !e.contains(x) && g.contains(x) =>
        B * e * g.extractC(x)
      case (g : Term) * (e : Term) if !e.contains(x) && g.contains(x) =>
        C * g.extractC(x) * e
      case (g : Term) * (h : Term) if g.contains(x) && h.contains(x) =>
        S * g.extractC(x) * h.extractC(x)

    }

    def contains(x: Var): Boolean = this match {
      case y : Const => false
      case y : Var => x == y
      case *(y,z) => y.contains(x) || z.contains(x)
    }
  }

  case class *(left: Term, right: Term) extends Term {
    override def toString = this match {
      case p * (q * r) => s"$p(${q * r})"
      case _ => s"$left$right"
    }
  }

  def eval(t: Term): Term = {
    if (evalHead(t) == t) t
    else eval(evalHead(t))
  }

  def evalHead(t: Term): Term = t match {
    case I * p => p
    case K * p * q => p
    case B * p * q * r => p * (q * r)
    case C * p * q * r => p * r * q
    case S * p * q * r => p * r * (q * r)
    case W * p * q => p * q * q
    case Bp * p * q * r * s => p * q * (r * s)
    case Cp * p * q * r * s => p * (q * s) * r
    case Sp * p * q * r * s => p * (q * s) * (r * s)
    case x : Const => x
    case x : Var => x
    case (x : Var) * xs => x * xs
    case (x : Term) * xs => eval(x) * eval(xs)
  }

  def bracketAbstraction(f: Term => Term): Term = {
    val x = Var("variable")
    f(x).extract(x)
  }

  def bracketAbstraction(f: (Term, Term) => Term): Term = {
    val x = Var("variable-x")
    val y = Var("variable-y")
    f(x, y).extract(y).extract(x)
  }

  sealed trait Atom extends Term
  sealed trait Const extends Atom
  case object S extends Const
  case object K extends Const
  case object I extends Const
  case object B extends Const
  case object C extends Const
  case object W extends Const
  case object Bp extends Const
  case object Cp extends Const
  case object Sp extends Const
  case object Y extends Const

  case class Var(name: String) extends Atom {
    override def toString = name
  }

  implicit def symbolToVar(s: Symbol) = new Var(s.name)
}
