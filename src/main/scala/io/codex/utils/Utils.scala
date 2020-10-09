package io.codex.utils

//import Application.logicalVariables
///
import io.codex.encoders.JsonSupport.TruthColumn

import scala.annotation.tailrec
import scala.collection.immutable.List

object Utils {

//  def conditionalWith
  def replaceConditional(xs:List[Char],conditionals:List[Int],leftParenthesis:List[Int],rightParenthesis:List[Int]) ={
    conditionals.map{
      i=>
        val value = xs(i)
        val next= xs(i+1)
        val prev = xs(i-1)
//        val prevList = xs.slice()
//        if( prev != '(' && next != ')')
    }
  }

  def characterWithIndexes(xs:List[Char],fn:Char=>Boolean): List[Int] =
    xs
      .zipWithIndex
      .filter(x=>fn(x._1))
      .map(_._2)

  def leftParenthesisWithIndexes(xs:List[Char]) = characterWithIndexes(xs,_=='(')
  def rightParenthesisWithIndexes(xs:List[Char])= characterWithIndexes(xs,_==')')
  def conditionalWithIndexes(xs:List[Char])= characterWithIndexes(xs,_=='>' ).filter {
    x=> xs(x-1) != '<'
  }
//
  @tailrec
  def biconditionalWithIndexes(n:Int=0,xs:List[Char], result:List[List[Int]]=Nil):List[List[Int]] = {
      val len = xs.length
      def find(n:Int):String = {
        val value= xs(n)
        if(len == n) return ""
        if(value =='<') s"$n-${find(n+1)}"
        else if(value == '>') n.toString
        else find(n+1)
      }
    if(len < 4) result
    else {
      val biMatch = find(0).split('-').map(_.toInt).toList
      if(biMatch.length <1) result
      def correctIndexes(x:List[Int]) = if(n>0) x.map(_+result(n-1).head) else x
//      println(result.map(_.last).sum,result,n)
      biconditionalWithIndexes(
        0,
        xs.slice(biMatch.last+1,xs.length),
        result:+biMatch.map(_+n))
    }
  }

//  def parenthesisWithIndexes(xs:List[Char]): List[(Int, Char)] =characterWithIndexes(xs, x=> x==')' || x=='(')
//  def conditionalWithIndexes(xs:List[Char]): List[(Int, Char)] =characterWithIndexes(xs, x=> x=='>')
//  def biconditionalWithIndexes(xs:List[Char])=characterWithIndexes(xs,x=>x=='<>')

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


