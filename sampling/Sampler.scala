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
//============== original code ====================
//  def sample_e(psddNode: PsddNode, evidence: Map[Int,Boolean]): Set[Int] = {
//// ===========collect psdd with evidence
//    val childrenBeforeParentsNodes = PsddQueries.childrenBeforeParents(psddNode)
//    childrenBeforeParentsNodes.foreach(_.baggage = null) /*reset baggage to null*/
//    childrenBeforeParentsNodes.foreach { node =>
//      node.baggage = node match {
//        case node: PsddLiteral => if (evidence.getOrElse(node.v, node.pos) == node.pos) Log.one else Log.zero
//        case node: PsddTrue => Log.one
//        case node: PsddDecision => node.elements.map(el => Log.multiply(el.prime.baggage.asInstanceOf[Double], el.sub.baggage.asInstanceOf[Double], el.theta)).reduce(Log.add)
//      }
//    }
//    val prob = psddNode.baggage.asInstanceOf[Double] /* transfer type to double*/
////    println("---------sampling with evidence here")
//
//    //    println(prob)
//    def sample2(psddNode: PsddNode): Set[Int] = {
//      var sum_node = scala.collection.immutable.Set[Int]()
//      val child_num = psddNode.elements.toList.size.abs
//      val element_list: List[PsddElement] = (psddNode.elements.toList)
//
//      //===========remember for the case will not  happen, if the  prob=0
////      println("child :" + child_num)
//      if (child_num == 1) {
////        println("testchild=0")
//        val prime = element_list(0).prime
//        val sub = element_list(0).sub
////        println("prime0: ", prime, "---sub0: ", sub)
//        sum_node ++= judgement_e(prime, sub)
//      }
//
//      else {
//        var probability_low_bound: Double = Double.NegativeInfinity
//        var ct: Integer = 0
//        var probability_high_bound: Double = Double.NegativeInfinity
//
//        val rnd = new scala.util.Random
//
////        println()
//        var new_p:Double = Double.NegativeInfinity
//        var a = new ListBuffer[Double]()
//        var tt: Integer = 0
//  //=====first normailize the probability
//        while (tt < (element_list).size) {
////          println("integer", tt)
//          val subpro = Log.multiply(element_list(tt).prime.baggage.asInstanceOf[Double], (element_list(tt).sub.baggage.asInstanceOf[Double]), element_list(tt).theta)
//          a += subpro
////          println("el: ", xx)
//          tt += 1
//          new_p = Log.add(subpro,new_p)
//        }
////        println(new_p)
//        val r = Log.multiply(log(rnd.nextFloat()), new_p)
////        println(r)
//        //============start to sampleing with children >1
//        for (i <- a) {
////          println("r= " + r)
//          probability_high_bound = Log.add(probability_high_bound, i)
////          println("probability_high_bound", probability_high_bound)
////          println("probability_low_bound", probability_low_bound)
//          if (r > probability_low_bound && r <= probability_high_bound) {
////            println(element_list)
//            val prime = element_list(ct).prime
//            val sub = element_list(ct).sub
////            println("prime/: ", prime, "---sub: ", sub)
////            println(sub.elements.map(_.prime), "--------------------", sub.elements.map(_.sub))
////            println("test")
//            sum_node ++= judgement_e(prime, sub)
//            /*how to jump out of loop after one operation*/
//          }
//          probability_low_bound = Log.add(probability_low_bound, i)
//          ct += 1
//        }
//        sum_node
//      }
//
//
//      def judgement_e(prime: PsddNode, sub: PsddNode): Set[Int] = {
//        //        println("start to Judgement prime and sub")
//        val vars_to_add_prime = prime match {
//          case prime: PsddDecision => sample2(prime)
//          case prime: PsddLiteral => Set(prime.literal)
//          case prime: PsddTrue => Set()
//        }
//        val vars_to_add_sub = sub match {
//          case sub: PsddDecision => sample2(sub)
//          case sub: PsddLiteral => Set(sub.literal)
//          case sub: PsddTrue => Set()
//        }
//        vars_to_add_prime ++ vars_to_add_sub
//      }
//      sum_node
//    }
//    val test123 = sample2(psddNode)
//    test123
//  }
//  ================  cleaner code===========
def sample_e(psddNode: PsddNode, evidence: Map[Int,Boolean]): Set[Int] = {
  // ===========collect psdd with evidence
  def sample2(psddNode: PsddNode): Set[Int] = {
    var sum_node = scala.collection.immutable.Set[Int]()
    val child_num = psddNode.elements.toList.size.abs
    val element_list: List[PsddElement] = (psddNode.elements.toList)
    if (child_num == 1) {
      val prime = element_list(0).prime
      val sub = element_list(0).sub
      sum_node ++= judgement_e(prime, sub)
    }
    else {
      var probability_low_bound: Double = Double.NegativeInfinity
      var ct: Integer = 0
      var probability_high_bound: Double = Double.NegativeInfinity
      val rnd = new scala.util.Random
      var new_p:Double = Double.NegativeInfinity
      var a = new ListBuffer[Double]()
      var tt: Integer = 0
      //=====first normailize the probability
      while (tt < (element_list).size) {
        val subpro = Log.multiply(element_list(tt).prime.baggage.asInstanceOf[Double], (element_list(tt).sub.baggage.asInstanceOf[Double]), element_list(tt).theta)
        a += subpro
        tt += 1
        new_p = Log.add(subpro,new_p)
      }
      val r = Log.multiply(log(rnd.nextFloat()), new_p)
      //============start to sampleing with children >1
      for (i <- a) {
        probability_high_bound = Log.add(probability_high_bound, i)
        if (r > probability_low_bound && r <= probability_high_bound) {
          val prime = element_list(ct).prime
          val sub = element_list(ct).sub
          sum_node ++= judgement_e(prime, sub)
        }
        probability_low_bound = Log.add(probability_low_bound, i)
        ct += 1
      }
      sum_node
    }

    def judgement_e(prime: PsddNode, sub: PsddNode): Set[Int] = {
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



  /**
    *
    * @param psdd
    * @param n
    * @return N samples without evidence
    */
  def sample_N(psdd: PsddNode, n: Int, Nnode: Int): mutable.Map[Int, Float] ={
    var dd = new ListBuffer[String]()
    val dictionary= scala.collection.mutable.Map[Int,Float]()
    for (i <- 1 to Nnode){
      dictionary += (i -> 0)
      dictionary += (-i -> 0)
    }
//    val tt0 = System.nanoTime()
    for (a <-1 to n){
      val b = sample(psdd)
      val list2= List.empty[Int] ++ b
      for (i <- list2){
        val new_i = dictionary.get(i)
        dictionary(i) = new_i.get+1
      }
    }
//    val tt1 = System.nanoTime()
//    println("--- " + (tt1 - tt0 )*scala.math.pow(10,-9)+ "second---")
    for ((k,v)<-dictionary){
      dictionary(k) = float2Float(v/n)
    }
    return dictionary
  }
  def sampleE_N (psdd: PsddNode, evidence: Map[Int,Boolean], n: Int, Nnode: Int): mutable.Map[Int, Float] ={
    // ===========collect psdd with evidence and add evidence first
    val childrenBeforeParentsNodes = PsddQueries.childrenBeforeParents(psdd)
    childrenBeforeParentsNodes.foreach(_.baggage = null) /*reset baggage to null*/
    childrenBeforeParentsNodes.foreach { node =>
      node.baggage = node match {
        case node: PsddLiteral => if (evidence.getOrElse(node.v, node.pos) == node.pos) Log.one else Log.zero
        case node: PsddTrue => Log.one
        case node: PsddDecision => node.elements.map(el => Log.multiply(el.prime.baggage.asInstanceOf[Double], el.sub.baggage.asInstanceOf[Double], el.theta)).reduce(Log.add)
      }
    }
    val prob = psdd.baggage.asInstanceOf[Double] /* transfer type to double*/
//============start to sample
    var dd = new ListBuffer[String]()
    val dictionary= scala.collection.mutable.Map[Int,Float]()
    for (i <- 1 to Nnode){
      dictionary += (i -> 0)
      dictionary += (-i -> 0)
    }
    for (a <-1 to n){
      val b = sample_e(psdd, evidence)
      val list2= List.empty[Int] ++ b
      for (el <- list2) {
        val new_i = dictionary.get(el)
        dictionary(el) = new_i.get+1
      }
    }
    for ((k,v)<-dictionary){
      dictionary(k) = float2Float(v/n)
    }
    return dictionary
  }
//  ================ Sub Function =============================
  def print_psdd(a: mutable.Map[Int, Float])={
    val len = a.size
    var dd = new ListBuffer[String]()
    for (i <- 1 to len/2){
      val list2 = List(i,":{-1: ",a(-i),"+1: ",a(i)).toString()
      dd += list2

    }
    println(dd)
  }
  def takeRandomN[A](n: Int, as: List[A]) ={scala.util.Random.shuffle(as).take(n)}




  def main(args: Array[String]): Unit = {

    println("CODE start")
    val psddPath = "/home/austin/IdeaProjects/testpyBN/pyBN/data/psdd/16node.psdd" //change this
    val vtreePath = "/home/austin/IdeaProjects/testpyBN/pyBN/data/vtree/16node.vtree" //change this
//    val psddPath = "/home/austin/IdeaProjects/Diversifiedsampling/samplingPsdds/plants.psdd" //change this
//    val vtreePath = "/home/austin/IdeaProjects/Diversifiedsampling/samplingPsdds/plants.vtree" //change this
    val sddMgr = new SddManager(Vtree.read(vtreePath)) //create sdd manager
    val vtree = VtreeNode.read(new File(vtreePath)) // read vtree
    val psddMgr = new PsddManager(sddMgr, true) //create psdd manager
    psddMgr.cache = false // make sure that the psdd manager does not use caching, because then nodes without data would be collapsed, which is useful for learning, but not what we need here.
    val emptyData = new Data(Array[Map[Int, Boolean]](), Array[Double](), Array[Int](), BitSet()) // make an empty dataset. Only needed because the library was made for learning where data is always required.
    val psdd = psddMgr.readPsdd(new File(psddPath), vtree, new DataSets(Array(emptyData, emptyData, emptyData))) // read in the psdd
    println("test sampling scala")
    val test = ("""\d+""".r findAllIn psddPath).toList
    val NumofBNnode = (test.head).toInt
//    val NumofBNnode:Int = 69  /run example case

//    debug start=============
//    val s = sample_N(psdd, 1000)
//    val ss = mutable.ListMap(s.toSeq.sortWith(_._1 < _._1):_*)
//    println(ss)
//    debug end============
    println("=== do discardsampling scala=======")
////    ===================test without evidence
//    val experiment = 40 //change
//    val num = 1000  //change
//    for (i<-1 to experiment){
//      if (i <= 9){val b = sample_N(psdd, num, NumofBNnode)}
//      else{
////      val tt0 = System.nanoTime()
//      val b = sample_N(psdd, num, NumofBNnode)
////      val tt1 = System.nanoTime()
//      println(i-1 + " experiment start without evidence")
////      println("--- " + (tt1 - tt0 )*scala.math.pow(10,-9)+ "second---")
//      val bb = mutable.ListMap(b.toSeq.sortWith(_._1 < _._1):_*)
//      println(bb)
//      }
//    }
//      println("finish sampling without evidence")
    //    ===================test with random evidence
    val vars = List(16,15,12,14,11,3,9,13)  // 1:shuffle 3: List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16) (16,15,12,14,11,3,9,13)List(1,2,3,4,5)
    val Num = 8 // take ** of random variables
//    # ********** above is the parameter *********** #

//    println(vars_Sub)
    val experiment = 50 //change
    val num = 1000  //change

    val bool = List(true, false)
    for (i<-1 to experiment) {
      if (i <= 10){val b = sampleE_N(psdd, Map.empty , num, NumofBNnode)}
      else {
        //        create dictionary
        val vars_Sub = takeRandomN(Num, vars)
        val evi = scala.collection.mutable.Map[Int, Boolean]()
        for (el <- vars_Sub) {
          evi += (el -> takeRandomN(1, bool).head)
        }
        val immutableEvi = evi.toMap
        val tt0 = System.nanoTime()
        val b = sampleE_N(psdd, immutableEvi, num, NumofBNnode)
        val tt1 = System.nanoTime()
        val bb = mutable.ListMap(b.toSeq.sortWith(_._1 < _._1): _*)
        println(i - 1 + " experiment start without evidence")
        println("--- " + (tt1 - tt0) * scala.math.pow(10, -9) + "second---")
        println(bb, "evidence:", immutableEvi)
      }
    }
      println("finish sampling with random evidence")





    //    =======test with evidence
//    val tt0 = System.nanoTime()
//    val s_e0 = sampleE_N(psdd,Map(2 -> true),1000) //draw  a sample from the psdd.
//    val tt1 = System.nanoTime()
//    val s_e = sample_e(psdd,Map(2 -> true))
//    val tt2 = System.nanoTime()
//    println("--- " + (tt1 - tt0 )*scala.math.pow(10,-9)+ "second--- for 1000 samples")
//    println("--- " + (tt2 - tt1 )*scala.math.pow(10,-9)+ "second--- for 1 samples")


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
