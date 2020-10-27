import org.scalatest.funsuite.AnyFunSuite
import io.codex.functions.Functions.{CartesianProductSubset, getCartesianProductSubsetInfo}
import io.codex.sets.Setx
import io.codex.utils.Utils.listToTuple

class FunctionTest  extends  AnyFunSuite{
  test("Functions"){
    val relations = Setx(("X","1")::("W","1")::("Y","2")::Nil)
    val domain = Setx("X"::"W"::"Y"::Nil)
    val codomain = Setx("1"::"2"::Nil)
    val response = getCartesianProductSubsetInfo(relations,domain,codomain)
    println(response)

      assert(true)
  }

  test("tuples"){
    val t = "1"::"2"::"3"::Nil
    val nt = listToTuple(t)
    println(nt)
    assert(true)
  }

}
