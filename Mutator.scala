package pGA

class Mutator extends Serializable  {
  def mutate(  theChromo:  Chromosome  )  :  Chromosome  =    {
    theChromo
  }
}


class InterChanger extends Mutator  {
  override def mutate(  theChromo:  Chromosome  )  :  Chromosome  =  {
   var RandomNumber  =  scala.util.Random
   var r1  =  RandomNumber.nextInt  (  theChromo.Genes.length-1  )
   var r2  =  RandomNumber.nextInt  (  theChromo.Genes.length-1  )
   while  (  r1  ==  r2  )  {
     r1  =  RandomNumber.nextInt  (  theChromo.Genes.length-1  )
     r2  =  RandomNumber.nextInt  (  theChromo.Genes.length-1  )
   }
   //println("For Mutation			"+r1+"					"+r2)
   //theChromo.Genes.foreach(println)
   var MutatedGenes: Array[Gene]    =    theChromo.Genes
   var  temp  :  Gene  =  MutatedGenes(r1)
   MutatedGenes(r1)  =  MutatedGenes(r2)
   MutatedGenes(r2)  =  temp
   //MutatedGenes.foreach(println)
   new Chromosome  (  theChromo.ID  ,  MutatedGenes  ,  theChromo.fitnessFunc  )
  }
}


class Reverser extends Mutator  {
  override def mutate(  theChromo:  Chromosome  )  :  Chromosome  =  {
   var r1  =  scala.util.Random.nextInt  (  theChromo.Genes.length-2  )
   var r2  =  r1+1
   //println("For Mutation			"+r1+"					"+(r1+1))
   var MutatedGenes: Array[Gene]    =    theChromo.Genes
   var  temp  :  Gene  =  MutatedGenes(r1)
   MutatedGenes(r1)  =  MutatedGenes(r2)
   MutatedGenes(r2)  =  temp
   //MutatedGenes.foreach(println)
   new Chromosome  (  theChromo.ID  ,  MutatedGenes  ,  theChromo.fitnessFunc  )
  }
}