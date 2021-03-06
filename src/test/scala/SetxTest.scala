import io.codex.sets.Setx
import org.scalatest.funsuite.AnyFunSuite

class SetxTest extends  AnyFunSuite{

  test("Cardinality of A"){
    val A = Setx(1::2::Nil)
    assert(A.cardinality == 2)
  }
  test("Union of A and B"){
    val A = Setx(1::2::Nil)
    val B = Setx(2::1::3::4::Nil)
    val union = A|B
    assert(union == Setx(1::2::3::4::Nil))
  }
  test("Intersection of A and B"){
    val A = Setx(1::2::Nil)
    val B = Setx(2::1::3::4::Nil)
    val union = A&B
    assert(union == Setx(1::2::Nil))
  }
  test("Difference of A and B"){
    val A = Setx(1::2::Nil)
    val B = Setx(2::1::3::4::Nil)
    val result = A-B
    println(result.members)
    assert(result == Setx.empty)
  }
  test("Difference of B and A"){
    val A = Setx("1"::"HOLA"::Nil)
    val B = Setx("1"::"PEDO"::"AAA"::"HOLA"::Nil)
    val result = B-A
    println(result.members)
    assert(result == Setx(3::4::Nil))
  }

  test("Power set of A"){
    val A = Setx(1::2::3::Nil)
    val res = Setx.powerset(A.members)
    val text = res.members.map(_.members).map(_.mkString(",")).map(x=>s"{$x}")
    println(text)
    assert(true)
  }
//  X
  test("Difference A - B"){
    val A = Setx(1::2::6::7::Nil)
    val B = Setx(1::2::3::Nil)
    val result = A-B

    val answer = Setx(6::7::Nil)
    assert(result == answer)
  }
  test("Partitions of a set"){
    val A = Setx(1::2::3::Nil)
    assert(true)
  }
  test("Cartesian product: A~*B"){
    val values = List.fill(5)(s"GOLA-${math.random()}")
    val A = Setx(values)
    val res = A ~* A.members
    val res2 = Setx.powerset(res)
    println(s"|A| =${values.length}")
    println(s"|AxB| = ${res.length}")
    println(s"|Ps(|AxB|)| = ${res2.cardinality}")
//    val text = res.mkString(",")
//    println(text)
    assert(true)
  }
  test("Symmetric difference: A-&"){
    val A = Setx("A"::"B"::"C"::Nil)
    val B = Setx("A"::"B"::"C"::"D"::Nil)
    val res = A -& B
    val res2 = B-&A
    println(res)
    println(res2)

    assert(res == Setx.of(3))
  }
  test("Proper subset"){
    val A = Setx(1::2::5::Nil)
    val B = Setx(1::2::3::4::Nil)
    val res =  A.isProperSubsetOf(B)
    val properSubset = A.getEqualElements(B)
    assert(res)
  }


  test("A is a subset of B, iff all elements in A belongs to B as well") {
    val A = Setx(1::2::Nil)
    val B = Setx(2::1::3::4::Nil)
    assert(A.subsetOf(B))
  }
  test("A and B are equal, iff A is a subset of B and B is a subset of A"){
    val A = Setx(1::2::Nil)
    val B = Setx(2::1::Nil)
    assert(A==B)
  }

  test("A | {}=A") {
    val A = Setx(1::2::Nil)
    val result = A|Setx.empty
    println(result)
    assert(result == A)
  }

  test("Are A and B disjoint?"){
    val A = Setx(1::2::Nil)
    val B = Setx(4::3::Nil)
    assert(Setx.areDisjoint(A,B))
  }
}
