package pGA

import pGA._
//import scala.collection.parallel.ParIterableLike.Foreach

/*
 * Chromosome will comprise of one or more genes.
 * Member Variables: GeneCount, Genes
 * Member Functions: X,+ 
 */
class Chromosome( Id:Double, AlleleVals:Array[Gene], f:Array[Gene]=>Double  ) extends Serializable  {
  def this(AlleleVals:Array[Gene])  =  this( 0, AlleleVals,  AlleleVals =>0 )
  def this( Id:Double, AlleleVals:Array[Gene])  =  this( Id, AlleleVals,  AlleleVals =>0 )
  val ID:Double = Id
  val GeneCount:Int=AlleleVals.length
  val Genes: Array[Gene]= AlleleVals
  val fitness  =  f(Genes)
  val fitnessFunc  =  f
     
   
  //  Single Point Crossover: +
  def +( that:Chromosome  )  =  {
    val mid=GeneCount/2
    var Id1  =  (this.ID+that.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    var Id2  =  (this.ID+that.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    val OffSpring1:Chromosome = new Chromosome( Id1,  Genes.slice(0, mid) ++ that.Genes.slice(mid, that.Genes.length ),  f)
    val OffSpring2:Chromosome = new Chromosome( Id2,  that.Genes.slice(0, mid) ++ Genes.slice(mid, that.Genes.length ),  f)
    //println(OffSpring1.toString())
    //println(OffSpring2.toString())
    (  OffSpring1,  OffSpring2  )
  }
  
  
  // Single Point Crossover with CutPoint
  def +( that:(Int,Chromosome)  ) = {
    that match{
      case(i,c)=>{
        val mid=i
        var Id1  =  (this.ID+c.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
        var Id2  =  (this.ID+c.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    
        val OffSpring1:Chromosome = new Chromosome(  Id1,  Genes.slice(0, mid) ++ c.Genes.slice(mid, c.Genes.length ),  f)
        val OffSpring2:Chromosome = new Chromosome(  Id2,  c.Genes.slice(0, mid) ++ Genes.slice(mid, c.Genes.length ),  f)
        //println(OffSpring1.toString())
        //println(OffSpring2.toString())
        (  OffSpring1,  OffSpring2  )
      }
    }
  }
  
  
  //  Uniform Crossover
  def UX(that:Chromosome)  :  (  Chromosome,  Chromosome  )  =  {
    var solutions  =  Uniform(  this.Genes,  that.Genes  )
    var Id1  =  (this.ID+that.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    var Id2  =  (this.ID+that.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    val OffSpring1:Chromosome = new Chromosome( Id1,  solutions._1,  f)
    val OffSpring2:Chromosome = new Chromosome( Id2,  solutions._2,  f)
    //println(OffSpring1.toString())
    //println(OffSpring2.toString())
    (  OffSpring1,  OffSpring2  )
  }
  
  
  // Uniformly Combine Individuals
  def Uniform(  Parent1:  Array[Gene],  Parent2:  Array[Gene]  )  :  (  Array[Gene],  Array[Gene])=  {
    if  (  Parent1.length  ==  1  )  {
      if  (  scala.util.Random.nextInt(2)  ==  1  )
        (  Parent1,  Parent2  )
      else
        (  Parent2,  Parent1  )      
    }
    else  {
      var part1  =  Uniform(  Parent1.slice(0, Parent1.length/2),  Parent2.slice(0, Parent2.length/2)  )
      var part2  =  Uniform(  Parent1.slice(Parent1.length/2, Parent1.length),  Parent2.slice(Parent2.length/2, Parent2.length)  )
      (  part1._1  ++  part2._1  ,  part1._2  ++  part2._2  )
    }
  }
  
  
  // Three Parent Crossover
  def P3X(that:Chromosome,  other:Chromosome  )  :  Chromosome  =  {
    var solution  =  ThreeParent(  this.Genes,  that.Genes,  other.Genes  )
    var Id1  =  (this.ID+that.ID)/2  +  (math floor scala.util.Random.nextDouble()  *100)/10000
    
    val OffSpring:Chromosome = new Chromosome( Id1,  solution,  f)
    
    //println(OffSpring.toString())
    
    OffSpring
  }
  
  
  // Create new Offspring from 3 Parents
  def ThreeParent(  Parent1:  Array[Gene],  Parent2:  Array[Gene],  Parent3:  Array[Gene]  )  :    Array[Gene]  =  {
    if  (  Parent1.length  ==  1  )  {
      if  (  Parent1.deep  ==  Parent2.deep  )
        Parent1
      else
        Parent3
    }
    else  {
      var part1  =  ThreeParent(  Parent1.slice(0, Parent1.length/2),  Parent2.slice(0, Parent2.length/2),  Parent3.slice(0, Parent3.length/2)  )
      var part2  =  ThreeParent(  Parent1.slice(Parent1.length/2, Parent1.length),  Parent2.slice(Parent2.length/2, Parent2.length),  Parent3.slice(Parent3.length/2, Parent3.length)  )
      part1  ++  part2
    }
  }
  
  
  // Create Chromosome String
  override def toString(): String  =  {
    ID  +  " #   "  +fitness  +  " *** "  +  Genes.mkString(", ")
  }
  

}







