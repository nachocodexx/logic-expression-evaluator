package io.codex.utils

//import Application.logicalVariables
///
import io.codex.encoders.JsonSupport.TruthColumn

import scala.annotation.tailrec
import scala.collection.immutable.List

object Utils {

//  def
  def toTruthColumns(xss:List[(String,List[Int])]): List[TruthColumn] = xss.map {
    case (str, value) => TruthColumn(str,value)
  }
  def zipVariableWithValues(variables:List[Char],values:List[List[Int]]): List[(String, List[Int])] = variables.zipWithIndex.map {
    case (c, i) => (c.toString,values.map(x=>x(i)))
  }
  def booleanToString(x:Boolean): String = if(x) "Yes" else "No"
//  def zipVariablesAndTruthValues(variables:List[Char],values:List[Int])= {
//    def loop()
//  }
  def pad(xs:List[Int], size:Int): List[Int] = {
    val xLen= xs.length
    if(xLen == size)  xs
    else  {
      val diff = size - xLen
      val padZeros:List[Int] = List.fill(diff)(0)
      padZeros.concat(xs)
    }

  }

  @tailrec
  def toBinary(n:Int, bin: List[Int] = List.empty[Int]):List[Int] = {
    if(n ==1 ) 1::Nil
    else if (n== 0) 0::Nil
    else if(n/2 == 1) 1:: (n % 2) :: bin
    else {
      val remainder = n % 2
      val quotient = n / 2
      toBinary(quotient, remainder::bin)
    }
  }


//  @tailrec
  def substituteValuesInExpression(postfixExpression:String,
                                   logicalVariablesLength:Int, logicalVariables:List[Char],
                                   allValues:List[List[Int]]
                               ):List[String] = {
    @tailrec
    def _replaceInExpression(n:Int, values:List[Int], str:String):String ={
      if(n==0) str.replaceAll(logicalVariables.head.toString,values.head.toString)
      else {
        val value = values(n - 1)
        val character = logicalVariables(n - 1)
        _replaceInExpression(n - 1, values, str.replaceAll(character.toString, value.toString))
      }
    }
    allValues.map(_replaceInExpression(logicalVariablesLength,_,postfixExpression))
  }
}


