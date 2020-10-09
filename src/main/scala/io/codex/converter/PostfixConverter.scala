package io.codex.converter

import scala.annotation.tailrec
import scala.collection.immutable.List

object PostfixConverter {
    //  def hasConditional(xs:List[Char]): Nothing = ???
    //  def hasBiconditional(xs:List[Char]): Nothing = ???

    def isVariable(x:Char):Boolean = (!isOperator(x)  && !isParenthesis(x))
    def isOperator(x:Char): Boolean = getPrecedence(x) != -1
    def isRightParenthesis(x:Char): Boolean =x match {
      case ')' => true
      case _=> false
    }

    def isLeftParenthesis(x:Char): Boolean =x match {
      case '(' => true
      case _=> false
    }
    def isParenthesis(x:Char): Boolean = x match {
      case ')'| '('  => true
      case _ => false
    }
    def getPrecedence(operator:Char): Int = operator match {
      case '+' => 0
//      case '>' => 1
      case '→' | '↔' => 1
      case '*' => 2
      case '~' => 3
      case _ => -1
    }

    @tailrec
    def infixToPostfix(expression:List[Char], stack:List[Char],postFix:String="",foundLeft:Boolean=true):String = {
      if(expression.isEmpty) return postFix
      val currentSymbol = expression.head
      val  expressionTail = expression.tail
      if(!foundLeft){
        val stackLast = stack.last
        val stackInit =stack.init
        if(isLeftParenthesis(stackLast)) infixToPostfix(expressionTail,stackInit,postFix)
        else infixToPostfix(expression,stackInit,postFix+stackLast,foundLeft = false)
      }
      else if(isOperator(currentSymbol) || isLeftParenthesis(currentSymbol)) {
        val lastOperator = stack.last
        val lastOperatorPrecedence = getPrecedence(lastOperator)
        val currentOperatorPrecedence  = getPrecedence(currentSymbol)
        val stackInit = stack.init
        if(isParenthesis(currentSymbol)) infixToPostfix(expressionTail, stack:+currentSymbol, postFix)
        else if(lastOperatorPrecedence > currentOperatorPrecedence)
          infixToPostfix(expressionTail,stackInit :+ currentSymbol,postFix+lastOperator)
        else infixToPostfix(expressionTail, stack:+ currentSymbol,postFix)
      }
      else if( isRightParenthesis(currentSymbol) ) infixToPostfix(expression,stack,postFix,foundLeft = false)
      else infixToPostfix(expressionTail,stack,postFix+currentSymbol)
    }

}


