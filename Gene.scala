package pGA

class Gene(geneVal: Double) extends Serializable   {
  
  val Allele:Double = geneVal
 
  def Mutate(newAllele: Double):Gene  =  new Gene(newAllele) 
  override def toString(): String = Allele.toString()
   
}