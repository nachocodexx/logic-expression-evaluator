package io.codex.calculator
import cats.data.State
import scala.collection.immutable.List

object PostfixCalculator {
  type CalcState[A] = State[List[A],A]
  def operator(f:(Int,Int)=>Int):CalcState[Int] = State[List[Int],Int] {
    case a :: b :: tail =>
      val result = f(a,b)
      (result::tail,result)
  }
  def unaryOperator():CalcState[Int] = State[List[Int],Int] {
    case head::tail =>
      val result = if(head ==1) 0 else 1
      (result::tail,result)
  }
  def operand (num:Int):CalcState[Int]  = State[List[Int],Int] { stack=>
    ( num :: stack , num)
  }
  def conditional():State[List[Int],Int]= State[List[Int],Int] {
    case a :: b :: tail =>
      val result = if(b==1 && a==0 ) 0 else 1
//      println(s"P: $a -> Q: $b = $result")
//      println(a::b::tail)
      (result::tail,result)
  }



  def biconditional():CalcState[Int] = State[List[Int],Int] {
    case a::b::tail=>
      val result = if((a==1 && b==1) || (a==0 && b==0)) 1 else 0 
      (result::tail,result)
  }
  def evalOne(sym:String):CalcState[Int] = sym match {
    case "+" =>operator((x,y)=>if(x==0) y else x)
    case "*" =>operator(_*_)
    case "~" => unaryOperator()
    case ">" => conditional()
    case "→" => conditional()
    case "↔" => biconditional()
    case _ => operand(sym.toInt)
  }
  def evalInput(expression:String): CalcState[Int] = {
    val list = expression.toList
    list.foldLeft(State.pure[List[Int],Int](0) ){
      (x,y)=> x.flatMap(_=>evalOne(y.toString))
    }
  }
}
