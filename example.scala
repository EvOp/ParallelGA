import org.apache.spark._
import pGA._

object prog1 {
  
 def fitnessFunc  (  Alleles:Array[Gene]  )  :  Double =  {
      var fitness:Double  =  0.0
      for(i <- 0 to Alleles.length-1)  {
        fitness  =  fitness  +  (  Alleles(i).Allele * Math.pow(-1, i)  )
      }
      fitness
      }
  
 def sphericalFunc  (  Alleles:Array[Gene]  )  :  Double =  {
   var fitness:Double  =  0.0
   if  (  Alleles.length  ==  1  )
     fitness  =  Math.pow(Alleles(0).Allele, 2)
   else
     fitness  =   sphericalFunc  (   Alleles.slice(0, Alleles.length/2)  )  +  sphericalFunc  (   Alleles.slice(Alleles.length/2,  Alleles.length  )  )
   fitness
 }
  
  def main(  args:  Array[String]  )  {
  
    //println("Program Started")
    
    val ri=new RandomInitializer(500, 100, 0,20,  sphericalFunc  )
    //println("First Completed")
    val parGA=new GA(  sphericalFunc,  ri,  "RANDOM",  "REVERSE",  "WEAKPARENT",  "MIN",  25  ,  12.5  ,  "MAX_GENS",  200 ,  5 )
       
  }
}