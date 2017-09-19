package pGA

import scala.io.Source
import pGA._


abstract class Initializer {
  
  //Initialize population size, Default is zero
  var Populaion:Int = 0
  
  //Type of Initializer, Random or File
  var InitType:Int=0
  
  //Chromosome Length
  var chromoLength=0;
  var chromoList:List[(Double, Chromosome)]=List()
  
  
}

class RandomInitializer(P:Int, CL:Int, minval:Int=0, maxval:Int=1000, f:Array[Gene]=>Double) extends Initializer {
  Populaion = P
  InitType=1

  
  val r = scala.util.Random
    println("I am in  "+r)
  for(i <-  Populaion to 1 by -1)  {
      val temp  =  List.fill(CL)( minval + r.nextInt( maxval-minval ) )  // Initialize the chromosomes with random values within specified range
      val randomAlleles  =  temp.map(  x => new Gene(x)  ).toArray
      val newchromo  =  new Chromosome(i, randomAlleles,  f  )  // Create a chromosome from Randomly created Alleles
      chromoList = (i.asInstanceOf[Double], newchromo) :: chromoList
      println(newchromo)
  }
  

}

class FileInitializer(P:Int, CL:Int, fname:String) extends Initializer {
  Populaion = P
  InitType = 2
  
  val listOfLines = Source.fromFile(fname).getLines.toList
}