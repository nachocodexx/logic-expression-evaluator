package io.codex.sets

import scala.annotation.tailrec
import io.codex.utils.Utils.{factorial, nCr}

import scala.+:
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.collection.mutable.ArrayBuffer

case class Setx[+A](members:List[A]) {
  def contains[B >: A](x: B): Boolean = members.contains(x)
//  def subsetOf[B >: A](xs: Setx[B]): Boolean = xs.count(members.contains(_)) == members.length
//  def subsetOf[B >: A](xs: Setx[B]): Boolean = (this&xs).members==this.members
//  {1,2}  {1,2,3}
//  {1,2} C {1,2,3}    ,   A|B = {1,2,3}  A & B = {1,2}
  def subsetOf[B >: A](xs: Setx[B]): Boolean = (this&xs).cardinality == this.cardinality
  def getEqualElements[B>:A](ys:Setx[B]): List[B] = ys.members.filter(this.contains(_))
  def isProperSubsetOf[B>:A](ys:Setx[B]): Boolean = this.subsetOf(ys) && (ys-this).cardinality>0
  def count(f: A => Boolean): Int = members.count(f)
  def isEmpty: Boolean = members.isEmpty
  def cardinality: Int = members.length
  def ~*[B>:A](xs:List[B]): List[(B, B)] =  {
    val state:ArrayBuffer[(B,B)] = ArrayBuffer.empty[(B,B)]
    @tailrec
    def createCartesianProduct(index:Int, xsIndex:Int):Unit = {
      if(index>=this.cardinality) ()
      else if(xsIndex>=xs.length) createCartesianProduct(index+1,0)
      else {
        val element= (this.members(index),xs(xsIndex))
        state.addOne(element)
        createCartesianProduct(index,xsIndex+1)
      }
    }
    createCartesianProduct(0,0)
    state.toList
  }
//  SYMM
  def -&[B>:A](ys:Setx[B]): Setx[B] = (this | ys) - (this&ys)
  def ==[B >: A](ys: Setx[B]): Boolean = this.subsetOf(ys) && ys.subsetOf(this)
  def |[B >: A](ys: Setx[B]): Setx[B] = Setx((members :++ ys.members).distinct)

  def &[B >: A](ys: Setx[B]): Setx[B] = {
    if (members.length < ys.members.length) Setx(members.filter(ys.contains).distinct)
    else
      Setx(ys.members.filter(members.contains).distinct)
  }

  def -[B >: A](ys: Setx[B]): Setx[B] = {
     Setx(members.filterNot(ys.contains))
  }


//    Setx(this.members.map(x=>x.asInstanceOf[Setx[A]].members))



  override def equals(obj: Any): Boolean = obj match {
    case s:Setx[Any] => this.members.equals(s.members)
    case _ => false
  }
  override def toString: String = s"{ ${this.members.mkString(",")} }"
}

//case  object EmptySetx extends Setx[Nothing]
object Setx{
  def apply[A](members: List[A]): Setx[A] = new Setx(members.distinct)
  def of[A](x:A)=new Setx[A](x::Nil)
  def empty[A]: Setx[A] = Setx[A](Nil)
  def singleton[A](x:A): Setx[A] =Setx.of(x)

//  Power set function List[A]=> Setx[Setx[A]]
  def powerset[A](x:List[A]): Setx[Setx[A]] = {
//    State variable
    val state:ArrayBuffer[List[A]]=ArrayBuffer.empty
//    recursive function
    def createSubset(subset:List[A], index:Int):Unit = {
//    base case
      if(index >= x.length ) {
        state.addOne(subset)
        return
      }
      createSubset(subset,index+1)
      createSubset(subset:+x(index),index+1)
    }
    createSubset(Nil,0)
    Setx(state.toList.map(Setx(_)))
  }
  def areDisjoint[B](x:Setx[B],y:Setx[B]): Boolean = (x&y).cardinality==0



//  A={1,2}
//  Ps(A)= if(!A.isEmpty){A} U Ps(A-{1}) U Ps(A-{2}) else A
//  A-{1}={2}
  //  A-{2}={1}
  // Ps({2}) =P(A-{1})= {{2}} U  {{}}
  // Ps({1}) =P(A-{2})= {{1}}
//  Ps(A)={{1,2}} U {{2} U {{1}}
}
