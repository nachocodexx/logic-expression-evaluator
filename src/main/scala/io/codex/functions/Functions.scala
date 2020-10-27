package io.codex.functions

import cats.effect.IO
import io.codex.encoders.JsonSupport.FunctionItemResponse
import io.codex.sets.Setx
import io.codex.utils.Utils.{booleanToString, booleanTupleToString}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Functions {

  type RelationInfo = (Boolean,Boolean,Boolean,Boolean)
  type OrderedPair = (String,String)
  type CartesianProductSubset = Setx[OrderedPair]
  type Relations =List[CartesianProductSubset]
  type OrderPairs = List[OrderedPair]


  def getDomain(xs:List[(String,String)]): IO[Setx[String]] = IO(Setx(xs.map(_._1)))
  def getCodomain(xs:List[(String,String)]): IO[Setx[String]] = IO(Setx(xs.map(_._2)))


  def getCartesianProductSubsetInfo(x:CartesianProductSubset,
                                    domain:Setx[String],
                                    codomain:Setx[String]):RelationInfo={

    val domainState:ArrayBuffer[String]=ArrayBuffer.empty
    val codomainState:ArrayBuffer[String]=ArrayBuffer.empty
    @tailrec
    def check(orderPairs: OrderPairs):RelationInfo = {
     if(orderPairs.isEmpty) {
       val domainStateLen =domainState.length
       val codomainStateLen = codomainState.length
       val codomainStateLenTemp = (codomainState.distinct).length
       val isFunction = domainStateLen == domain.cardinality
       val isInjective =  isFunction && codomain.cardinality == codomainStateLen
       val isSurjective = isFunction && (codomain.cardinality == codomainStateLenTemp)
       return (isFunction,
         isInjective,
         isSurjective,
          isInjective && isSurjective
       )
     }
      val orderedPair = orderPairs.head
      val domainOrderedPair = orderedPair._1
      val codomainOrderedPair = orderedPair._2
      if(domainState.contains(domainOrderedPair)) (false,false,false ,false)
      else {
        domainState.addOne(domainOrderedPair)
        codomainState.addOne(codomainOrderedPair)
        check(orderPairs.tail)
      }


    }

    check(x.members)
  }

  def processRelations(relations: Relations,domain:Setx[String],codomain:Setx[String]):IO[List[FunctionItemResponse]]=
    IO(relations.map{
      x=>
        val relationInfo = getCartesianProductSubsetInfo(x,domain,codomain)
        val relationInfoStr = booleanTupleToString(relationInfo)
        FunctionItemResponse(x.members,"Yes",relationInfoStr._1,
          relationInfoStr._2,
          relationInfoStr._3,
          relationInfoStr._4)
    })



}
