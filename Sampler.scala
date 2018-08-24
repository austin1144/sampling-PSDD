package sampling

import java.io.File

import operations.PsddQueries

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//import com.sun.org.glassfish.external.statistics.Statistic
import util.Log
import operations.PsddManager
import sdd.{SddManager, Vtree}
import structure._

import scala.math._
import scala.math.log
import scala.collection.immutable.BitSet
/**
  * Created by jessa on 12/20/17.
  */

object Sampler {

  def judgement(prime: PsddNode, sub: PsddNode): Set[Int] = {
//        println("start to Judgement prime and sub")
    val vars_to_add_prime = prime match {
      case prime: PsddDecision => sample(prime)
      case prime: PsddLiteral => Set(prime.literal)
      case prime: PsddTrue => Set()
    }
    val vars_to_add_sub = sub match {
      case sub: PsddDecision => sample(sub)
      case sub: PsddLiteral => Set(sub.literal)
      case sub: PsddTrue => Set()
    }
    vars_to_add_prime ++ vars_to_add_sub
  }

  //===========================normal sample ================================
  def sample(psddNode: PsddNode): Set[Int] = {
    var sum_node = scala.collection.immutable.Set[Int]()
    //   because this is a recursive, so after the sample choose first node, all the variable will be instantiate to literal
    val child_num = psddNode.elements.size.abs
    val element_list: List[PsddElement] = (psddNode.elements.toList)

    //    println("child :"+ child_num)
    if (child_num == 1) {
      val prime = element_list(0).prime
      val sub = element_list(0).sub
      sum_node ++= judgement(prime, sub)
    }

    else {
      var probability_low_bound: Double = Double.NegativeInfinity
      var ct: Integer = 0
      var probability_high_bound: Double = Double.NegativeInfinity
      val rnd = new scala.util.Random
      val r = log(rnd.nextFloat())
      //      println(psddNode.elements)
      //      println(psddNode.elements.toList.map(_.theta))
      for (i <- psddNode.elements.toList.map(_.theta)) {
        probability_high_bound = Log.add(probability_high_bound, i)
        if (r > probability_low_bound && r <= probability_high_bound) {
          val prime = element_list(ct).prime
          val sub = element_list(ct).sub
          sum_node ++= judgement(prime, sub)
          /*how to jump out of loop after one operation*/
        }
        probability_low_bound = Log.add(probability_low_bound, i)
        ct += 1
      }
    }
    sum_node
  }

//===========================sample with evidence================================

  def sample_e(psddNode: PsddNode, evidence: Map[Int,Boolean]): Set[Int] = {
// ===========collect psdd with evidence
    val childrenBeforeParentsNodes = PsddQueries.childrenBeforeParents(psddNode)
    childrenBeforeParentsNodes.foreach(_.baggage = null) /*reset baggage to null*/
    childrenBeforeParentsNodes.foreach { node =>
      node.baggage = node match {
        case node: PsddLiteral => if (evidence.getOrElse(node.v, node.pos) == node.pos) Log.one else Log.zero
        case node: PsddTrue => Log.one
        case node: PsddDecision => node.elements.map(el => Log.multiply(el.prime.baggage.asInstanceOf[Double], el.sub.baggage.asInstanceOf[Double], el.theta)).reduce(Log.add)
      }
    }
    val prob = psddNode.baggage.asInstanceOf[Double] /* transfer type to double*/
//    println("---------sampling with evidence here")

    //    println(prob)
    def sample2(psddNode: PsddNode): Set[Int] = {
      var sum_node = scala.collection.immutable.Set[Int]()
      val child_num = psddNode.elements.toList.size.abs
      val element_list: List[PsddElement] = (psddNode.elements.toList)

      //===========remember for the case will not  happen, if the  prob=0
//      println("child :" + child_num)
      if (child_num == 1) {
//        println("testchild=0")
        val prime = element_list(0).prime
        val sub = element_list(0).sub
//        println("prime0: ", prime, "---sub0: ", sub)
        sum_node ++= judgement_e(prime, sub)
      }

      else {
        var probability_low_bound: Double = Double.NegativeInfinity
        var ct: Integer = 0
        var probability_high_bound: Double = Double.NegativeInfinity

        val rnd = new scala.util.Random

//        println()
        var new_p:Double = Double.NegativeInfinity
        var a = new ListBuffer[Double]()
        var tt: Integer = 0
  //=====first normailize the probability
        while (tt < (element_list).size) {
//          println("integer", tt)
          val subpro = Log.multiply(element_list(tt).prime.baggage.asInstanceOf[Double], (element_list(tt).sub.baggage.asInstanceOf[Double]), element_list(tt).theta)
          a += subpro
//          println("el: ", xx)
          tt += 1
          new_p = Log.add(subpro,new_p)
        }
//        println(new_p)
        val r = Log.multiply(log(rnd.nextFloat()), new_p)
//        println(r)
        //============start to sampleing with children >1
        for (i <- a) {
//          println("r= " + r)
          probability_high_bound = Log.add(probability_high_bound, i)
//          println("probability_high_bound", probability_high_bound)
//          println("probability_low_bound", probability_low_bound)
          if (r > probability_low_bound && r <= probability_high_bound) {
//            println(element_list)
            val prime = element_list(ct).prime
            val sub = element_list(ct).sub
//            println("prime/: ", prime, "---sub: ", sub)
//            println(sub.elements.map(_.prime), "--------------------", sub.elements.map(_.sub))
//            println("test")
            sum_node ++= judgement_e(prime, sub)
            /*how to jump out of loop after one operation*/
          }
          probability_low_bound = Log.add(probability_low_bound, i)
          ct += 1
        }
        sum_node
      }


      def judgement_e(prime: PsddNode, sub: PsddNode): Set[Int] = {
        //        println("start to Judgement prime and sub")
        val vars_to_add_prime = prime match {
          case prime: PsddDecision => sample2(prime)
          case prime: PsddLiteral => Set(prime.literal)
          case prime: PsddTrue => Set()
        }
        val vars_to_add_sub = sub match {
          case sub: PsddDecision => sample2(sub)
          case sub: PsddLiteral => Set(sub.literal)
          case sub: PsddTrue => Set()
        }
        vars_to_add_prime ++ vars_to_add_sub
      }
      sum_node
    }
    val test123 = sample2(psddNode)
    test123
  }
  def sample_N(psdd: PsddNode, n: Int): mutable.Map[Int, Float] ={
    var dd = new ListBuffer[String]()
    val dictionary= scala.collection.mutable.Map[Int,Float]()

    for (a <-1 to n){
      val b = sample(psdd)
      val list2= List.empty[Int] ++ b
      val len_list = list2.length
      for (i <- list2){
        if (dictionary contains(i)){
          val new_i = dictionary.get(i)
          dictionary(i) = new_i.get+1
        }
        else{
          dictionary += ( i -> 1)
        }
      }
    }
    for ((k,v)<-dictionary){
      dictionary(k) = float2Float(v/n)
    }
    return dictionary
  }
  def sampleE_N (psdd: PsddNode, evidence: Map[Int,Boolean], n: Int): mutable.Map[Int, Float] ={
    var dd = new ListBuffer[String]()
    val dictionary= scala.collection.mutable.Map[Int,Float]()

    for (a <-1 to n){
      val b = sample_e(psdd,evidence)
      val list2= List.empty[Int] ++ b
      val len_list = list2.length
      for (i <- list2){
        if (dictionary contains(i)){
          val new_i = dictionary.get(i)
          dictionary(i) = new_i.get+1
        }
        else{
          dictionary += ( i -> 1)
        }
      }
    }
    for ((k,v)<-dictionary){
      dictionary(k) = float2Float(v/n)
    }
    return dictionary
  }

  def print_psdd(a: mutable.Map[Int, Float])={
    val len = a.size
    for (i <- 1 to len/2){
      println(i,":{-1: ",a(-i),"+1: ",a(i))
    }

  }

  def main(args: Array[String]): Unit = {

    println("CODE start")
    val psddPath = "/home/austin/IdeaProjects/testpyBN/pyBN/data/psdd/8node.psdd" //change this
    val vtreePath = "/home/austin/IdeaProjects/testpyBN/pyBN/data/vtree/8node.vtree" //change this
//    val psddPath = "/home/austin/IdeaProjects/Diversifiedsampling/samplingPsdds/nltcs.psdd" //change this
//    val vtreePath = "/home/austin/IdeaProjects/Diversifiedsampling/samplingPsdds/nltcs.vtree" //change this
    val sddMgr = new SddManager(Vtree.read(vtreePath)) //create sdd manager
    val vtree = VtreeNode.read(new File(vtreePath)) // read vtree
    val psddMgr = new PsddManager(sddMgr, true) //create psdd manager
    psddMgr.cache = false // make sure that the psdd manager does not use caching, because then nodes without data would be collapsed, which is useful for learning, but not what we need here.
    val emptyData = new Data(Array[Map[Int, Boolean]](), Array[Double](), Array[Int](), BitSet()) // make an empty dataset. Only needed because the library was made for learning where data is always required.
    val psdd = psddMgr.readPsdd(new File(psddPath), vtree, new DataSets(Array(emptyData, emptyData, emptyData))) // read in the psdd

    val t0 = System.nanoTime()
//    val s = sampleE_N(psdd,Map(2 -> true),3000) //draw  a sample from the psdd.
    val s = sample_N(psdd,3000) //draw  a sample from the psdd.
    val t1 = System.nanoTime()
    print_psdd(s)
    println("---" + (t1 - t0)*scala.math.pow(10,-9)+ " second---")
    println("finish sampling scala")
//    =======test with evidence
//    val s_e = sample_e(psdd,Map(2 -> true))
//    print(s_e)
//    ===================test with evidence
//      val experiment=30
//      var dd = new ListBuffer[String]()
//      val num = 1000
//  //    for (i<-1 to experiment){
//      val t0 = System.nanoTime()
//
//      for (a <-1 to num){
//      val b = sample_e(psdd,Map(2 -> true))
//      val list2=   b.toString()
//      dd += list2
//                }
//      val t1 = System.nanoTime()
//
//      println("--- " + (t1 - t0 )*scala.math.pow(10,-9)+ "second---")
//  //    }
//      println("finish sampling with evidence")
//      val a2 = dd.groupBy(identity).mapValues(_.size)("Set(-1, 2)")
//      val a3 = dd.groupBy(identity).mapValues(_.size)("Set(1, 2)")
//      println("ratio of (-1, 2): " + a2.toFloat/num)
//      println("ratio of (1,2): " + a3.toFloat/num)
//      println(dd)
    //   ===============test running time---------
//        var dd = new ListBuffer[String]()
//        val num = 1000
//        val t0 = System.nanoTime()
//
//        for (a <-1 to num){
//          val b = sample(psdd)
//          val list2=   b.toString()
//          dd += list2
//                }
//        val t1 = System.nanoTime()
//
//        println("---" + (t1 - t0)*scala.math.pow(10,-9)+ "second---")
//        println("finish sampling scala")
//        println(dd)




    /*================= test uniform distribution*/
    //    println("statistical")
    //        var dd = new ListBuffer[String]()
    //        val num = 1000
    //        for (a <-1 to num){
    //          val b = sample(psdd)
    //          val list2=   b.toString()
    //          dd += list2
    //        }
    //        println(dd)
    //        val a1 = dd.groupBy(identity).mapValues(_.size)("Set(-1, -2)")
    //        val a2 = dd.groupBy(identity).mapValues(_.size)("Set(-1, 2)")
    //        val a3 = dd.groupBy(identity).mapValues(_.size)("Set(1, 2)")
    ////        val a3 = dd.groupBy(identity).mapValues(_.size)("Set(-1, -2)")
    //        println("ratio of (-1,-2): " + a1.toFloat/num)
    //        println("ratio of (-1, 2): " + a2.toFloat/num)
    //        println("ratio of (1,2): " + a3.toFloat/num)
    //        println("ratio of (1,-2): "+  (num-a1-a2-a3)  )
    //        println("done")
    /* finish test uniform distribution*/
  }
}
