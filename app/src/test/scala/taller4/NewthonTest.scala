package taller4
import org.scalatest.funsuite.AnyFunSuiteLike
class NewthonTest extends AnyFunSuiteLike {

 test("mostrar expr1") {
   val expr1 = Suma(Atomo('x'), Numero(2.0))
   assert(Newton.mostrar(expr1) == "(x + 2.0)")
 }

  test("mostrar expr2") {
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    assert(Newton.mostrar(expr2) == "(x * x)")
  }

  test("mostrar expr3") {
    val expr1 = Suma(Atomo('x'), Numero(2.0))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5.0)))
    assert(Newton.mostrar(expr3) == "((x + 2.0) + ((x * x) ^ 5.0))")
  }


}
