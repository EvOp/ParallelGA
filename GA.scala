package pGA

import org.apache.spark._
import pGA._
import org.apache.spark.rdd.RDD

class GA(  f:  Array[Gene]  =>  Double  , init:Initializer  ,
    SelectorType:String  ,  MutatorType:String  ,  Replacement:String  ,  direction:String  ,
    CrossOverProb:Double  ,  MutationProb:Double  ,
    stopper_Type:String  ,  Stopper_Threshold:Long  ,  PartitionsCount:Int    )   extends  Serializable  {
  
  
  //Create Spark Configurations
  val conf = new SparkConf().setAppName("Parallel GA").setMaster("local[*]")  // local mode
    
  //Create Spark Context
  val sc = new SparkContext(conf)
    
  
  
  
  //  Parameters PreProcessing to appropriate objects
  var chromoList:List[(Double, Chromosome)]=List()
  init.InitType match{
      case 1 => {  chromoList  =  init.asInstanceOf[RandomInitializer].chromoList  }  //IntRDDCreation()
      case 2 => {  chromoList  =  init.asInstanceOf[RandomInitializer].chromoList  }  //FileRDDCreation()
  }
    
  val theSelector  =  SelectorType match{
    case "RANDOM"    =>  new RandomSelector(  sc  ,  25  ,  MutatorType  ,  Replacement  ,  direction  ,  CrossOverProb  ,  MutationProb  )
  }
  //  End Parameters PreProcessing
  
  //Intialize Generation Number to 1
  var gens:Int  =  1
  
  //RDD Creation from List
  val chromoRDD = sc.parallelize(chromoList)
  
  // Create Range Partitioner
  val TunedPartitioner  =  new RangePartitioner  (  PartitionsCount  ,  chromoRDD  )
  
  // Partition RDD according to Range partitioner
  val Partitioned  =  chromoRDD.partitionBy  (  TunedPartitioner  ).persist(  )
  Partitioned.collect()
  
  //Define the Gap after which Best Solution of a Partition will replace the weak solution of other partition
  var gap:Int  =  2
  
    
  var nextPartitions  =  Partitioned
    
  //println("Starting While")
  
  val  theStopper:Stopper  =  new Stopper(stopper_Type,Stopper_Threshold)
  
  while(  !(  theStopper.stop(gens,0)  )  )  {
    
    nextPartitions   =  theSelector.selection(nextPartitions,  5,  30)
    
    //nextPartitions.map(x=>x._2).foreach(println)
    //println("Before Selection")
    if(  gens  %  gap  ==  gap/2  )  {
      
      //println("BroadCasting")
      var BestofBest  =  theSelector.selectBest(nextPartitions)
      println("The Best Solution After Generation "+gens+" is "+BestofBest )
      
    }
    else  if(  gens  %  gap  ==  0)  {
      //println("Recieving")
      nextPartitions  =  theSelector.eliminateWeak(nextPartitions)
      nextPartitions.collect()
    }
    gens  +=  1
  }
  

}