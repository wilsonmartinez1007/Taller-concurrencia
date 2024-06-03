package taller4

trait Expr
case class Numero(d: Double) extends Expr
case class Atomo(x: Char) extends Expr
case class Suma(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Resta(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Expo(e1: Expr, e2: Expr) extends Expr
case class Logaritmo(e1: Expr) extends Expr

object Newton {
  // Function to show the expression as a string
  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(log(${mostrar(e1)}))"
  }

  // Function to derive the expression with respect to the given atom
  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0.0)
    case Atomo(x) if x == a.x => Numero(1.0)
    case Atomo(_) => Numero(0.0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Prod(derivar(e1, a), Div(e2, e1)), Prod(derivar(e2, a), Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  // Function to evaluate the expression at a given value of the atom
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
  }

  // Function to clean up the expression by removing unnecessary zeros and ones
  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0.0), e) => limpiar(e)
    case Suma(e, Numero(0.0)) => limpiar(e)
    case Prod(Numero(1.0), e) => limpiar(e)
    case Prod(e, Numero(1.0)) => limpiar(e)
    case Prod(Numero(0.0), _) => Numero(0.0)
    case Prod(_, Numero(0.0)) => Numero(0.0)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case _ => f
  }

  // Function to find the root using Newton's method
  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    val maxIterations = 1000
    val tolerance = 1e-10
    def iterate(xi: Double, iterations: Int): Double = {
      if (iterations >= maxIterations) xi
      else {
        val fx = evaluar(f, a, xi)
        val dfx = evaluar(derivar(f, a), a, xi)
        val xi1 = xi - fx / dfx
        if (ba(f, a, xi1)) xi1
        else iterate(xi1, iterations + 1)
      }
    }
    iterate(x0, 0)
  }

  // Boolean function to check if the approximation is good
  // Boolean function to check if the approximation is good
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = Math.abs(evaluar(f,a,d))<0.001



}