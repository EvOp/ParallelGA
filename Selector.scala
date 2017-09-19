package pGA
import org.apache.spark._
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import pGA._
import scala.collection.parallel._
import org.apache.xpath.FoundIndex
import org.apache.spark.streaming._
import java.io._
import sun.security.provider.NativePRNG.Blocking
import org.apache.spark.broadcast
import org.apache.spark.broadcast.Broadcast
import scalax.collection.GraphTraversal.Direction



abstract class Selector  extends Serializable{
  
//  val crossoverProbability  =  sc.broadcast  (  crossProbability  )
//  val theMutator  =  sc.broadcast  (    MutatorType match  {
//    case  "INTERCHANGE"  =>  new InterChanger
//    case  "REVERSE"      =>  new Reverser
//  }    )
  
//  val Direction  =  sc.broadcast  (  Direct  )
    
  var SelectorType:Int  =  0
  def selection(rdd: RDD[  (Double, Chromosome) ],  generation:Int,  crossProbability:Int): RDD[  (Double, Chromosome)]  =   {
      rdd
  }
  def selectBest  (  Partitioned: RDD[  (Double, Chromosome)  ]  )    :  Chromosome  =  {
      var p  =  Partitioned.take(1)
      p(0)._2
   }
   def eliminateWeak  (  Partitioned: RDD[  (Double, Chromosome)  ]  ): RDD[  (Double, Chromosome)]  =  {
     Partitioned
   }
    
}



class RandomSelector(  sc:SparkContext  , crossProbability:Double  ,  MutatorType:String  ,  
    ReplaceScheme:String,  Direct:String  ,  CrossOverProb:Double  ,  MutationProb:Double  ) extends Selector   {
  

  //val ReplacementScheme  =  ReplaceScheme
  SelectorType  =  1
  //val reproductionCount  =  reproductionProbability  /  100  
  val ssc = new StreamingContext(sc, Seconds(1))
  val initSolution  =  sc.parallelize(  List(  (0.0,new Chromosome(0.0,Array(new Gene(0)))),  (0.0,new Chromosome(0.0,Array(new Gene(0))))    )  )
  //var bestSolutions  =  sc.broadcast(  initSolution.collect()  )
  var bestSolutions  =  BroadcastWrapper(  ssc,  initSolution.collect()  )
  //println("Broadcasted")
  
  
  override def selection(  Partitioned: RDD[  (Double, Chromosome)  ],  generation:Int,  
      crossProbability:Int) : RDD[  (Double, Chromosome)] =  {
    
    val ReplacementScheme  =  ReplaceScheme  
    val theMutator  =  MutatorType match  {
      case  "INTERCHANGE"  =>  new InterChanger
      case  "REVERSE"      =>  new Reverser
    }
    val Direction  =  Direct
    val  CrossOverProbability  =  CrossOverProb
    val  MutationProbability  =  MutationProb
    
    val mapped  =  Partitioned.mapPartitionsWithIndex{
      (index, Iterator)  => {
            
        var myArray  =  Iterator.toArray
        var RandomNumber  =  scala.util.Random
        
        
        def BothParent(  P1:Chromosome,  P2:Chromosome,  O1:Chromosome,  O2:Chromosome)  =  {
            (O1,O2)
        }
          def WeakParent(P1:Chromosome,  P2:Chromosome,  O1:Chromosome,  O2:Chromosome)  =  {
          var ChromoArray:Array[Chromosome]  =  Array(  P1,  P2,  O1,  O2  )
          //ChromoArray.foreach(println)
          var fitnessArray:Array[Double]  =  ChromoArray.map(x=>x.fitness)
          
          //println("Is Array Sorted  ????????????")
          
          if  (  Direction  ==  "MAX"  )
            ChromoArray  =  ChromoArray.sortWith(  _.fitness  >  _.fitness  )
          else
            ChromoArray  =  ChromoArray.sortWith(  _.fitness  <  _.fitness  )
          
          //ChromoArray.foreach(println)
            
          (  ChromoArray(0)  ,  ChromoArray(1)  )
          
        }
        
        var r1  =  RandomNumber.nextInt(myArray.length-1)
        var r2  =  RandomNumber.nextInt(myArray.length-1)
        while  (  r1  ==  r2  )  {
          r1  =  RandomNumber.nextInt(myArray.length-1)
          r2  =  RandomNumber.nextInt(myArray.length-1)
        }
        var Parent1  =  myArray(r1)
        var Parent2  =  myArray(r2)
        var OffSprings  =  Parent1._2  +  Parent2._2
        //println("************************************************")
        //println(r1+"----------"+r2)
        
          
        var crossovers:Int  =  (CrossOverProbability*myArray.length/100).toInt  //myArray.length*reproductionCount/100
        var mutations:Int  =  (MutationProbability*myArray.length/100).toInt
        //println("Total Crossovers	" +crossovers  +"	Total Mutations	"+mutations)
        for (i  <-  1  to crossovers  ){
          
          var replacement  =  ReplacementScheme match  {
          case  "BOTHPARENT"  =>  BothParent  (  Parent1._2  ,  Parent2._2  ,  OffSprings._1  ,  OffSprings._2  )
          case  "WEAKPARENT"  =>  WeakParent  (  Parent1._2  ,  Parent2._2  ,  OffSprings._1  ,  OffSprings._2  )
          }
          myArray(r1)  =  (  replacement._1.ID,  replacement._1  )
          myArray(r2)  =  (  replacement._2.ID,  replacement._2  )
          
          r1  =  RandomNumber.nextInt(myArray.length-1)
          r2  =  RandomNumber.nextInt(myArray.length-1)
          Parent1  =  myArray(r1)
          Parent2  =  myArray(r2)
          OffSprings  =  Parent1._2  +  Parent2._2
        }
        
        //////// Do this as many times as is the mutation ratio
        for (i  <-  1  to mutations  ){
          r1  =  RandomNumber.nextInt(myArray.length-1)
          var ToMutate  =  myArray(r1)._2
          //println("BEFORE MUTATION						"+ToMutate)
          var Mutated  =  theMutator.mutate(  ToMutate )
          myArray(r1)  =  (Mutated.ID,  Mutated  )
          //println("AFTER MUTATION						"+Mutated)
        }
        //////////
        myArray.iterator
      }
    }
   mapped 
  }
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  override def selectBest  (  Partitioned: RDD[  (Double, Chromosome)  ]    )  :  Chromosome  =  {
    val Direction  =  Direct
    //println("Ready to Broadcast")
     val mapped  =  Partitioned.mapPartitionsWithIndex{
      (index, Iterator)  => {
        
        var myArray  =  Iterator.toArray
        var thyArray  =  myArray.map(x=>x._2.fitness)
        var foundindex= Direction match  {
          case  "MAX"  =>  thyArray.indexOf(thyArray.max)
          case  "MIN"  =>  thyArray.indexOf(thyArray.min)
        }
        myArray(foundindex)  =  (index,  myArray(foundindex)._2)
        var LocalOptima:Array[(Double, Chromosome)]  =  Array(myArray(foundindex))
        LocalOptima.iterator
      }
    } 
   //println("Array collected successfully")
   var SelectedBests  =  mapped.collect()
   //var temp1  =  sc.parallelize(temp)
   bestSolutions.update(  SelectedBests  ,  true  )
   //println("DIRECTION IS                                                      "+Direction)
   if (  Direction  ==  "MAX"  )
     SelectedBests  =  SelectedBests.sortWith(  _._2.fitness  >  _._2.fitness  )
   else
     SelectedBests  =  SelectedBests.sortWith(  _._2.fitness  <  _._2.fitness  )
   var BestOfBest    =    SelectedBests(0)
   BestOfBest._2
  }
   
  
  override def eliminateWeak  (  Partitioned: RDD[  (Double, Chromosome)  ]  ): RDD[  (Double, Chromosome)]  =  {
    val Direction  =  Direct
    
    var RecievedBroadCast  =  bestSolutions.value  
  
    val mapped  =  Partitioned.mapPartitionsWithIndex  {
      (index, theIterator)  => {
        var myArray  =  theIterator.toArray
        var thyArray  =  myArray.map(x=>x._2.fitness)
        //println(" >>>>>>>>>>>>>>>>>>>>>>> ")
        //RecievedBroadCast.map(x=>x._2).foreach(println)
        //println(" >>>>>>>>>>>>>>>>>>>>>>> ")
        
        
        RecievedBroadCast  =  RecievedBroadCast.filter(_._1  ==  (  (index+1)  %  5  )  )
        var required  =  RecievedBroadCast.take(1)
        //println("Current Index is "+index+" Recieved index is "+required(0)._1+"  and solusion is  "+required(0)._2)
        
        var foundindex= Direction match  {
          case  "MAX"  =>  thyArray.indexOf(thyArray.min)
          case  "MIN"  =>  thyArray.indexOf(thyArray.max)
        }
        
        myArray  (  foundindex  )  =  (  required(0)._2.ID  ,  required(0)._2  )
        
        myArray.iterator
      }
    }
   mapped
  }
  
}