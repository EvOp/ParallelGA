package pGA

class Stopper(Criteria_Type:String,  Thresh:Long) {
  
  //  Criteria =  MAX_GENS  /  ELAPSED_TIME  /  FITNESS_UNCHANGE  /  STALL_GENS  /  STALL_TIME
  val Criteria:String  =  Criteria_Type
  val Threshold:Long  =  Thresh
  val t0  =  System.nanoTime()
  var  lastFitness  :  Double  =  0.0
  var staticGens  =  0
  
  def stop(CurrGen  :  Long  ,  currFitness  :  Double  )  :  Boolean  =  {
    // Conditional Check on MAX_GENS
    if  (  Criteria  ==  "MAX_GENS"  )  
      if(  CurrGen  <  Threshold  )
        return false
    // Conditional Check on ELAPSED_TIME
    if  (  Criteria  ==  "ELAPSED_TIME"  )  
      if(  System.nanoTime()  -  t0  <  Threshold  )
        return false
    // Conditional Check on FITNESS_UNCHANGE
    if  (  Criteria  ==  "FITNESS_UNCHANGE"  )  {
      if(  currFitness  !=  lastFitness )  {
        staticGens  =  0
        return false
      }
      else  {
        staticGens  +=  1
        if  (  staticGens  <  Threshold  )
          return false
      }
    }
    // Conditional Check on STALL_GENS
    // Conditional Check on STALL_TIME
        
    val t1  =  System.nanoTime()
    println("Total Time Consumed		:		"+  (  (t1-t0).toDouble  /  1000000000  ).toDouble  +"	Seconds")
    return true
      
  }
  
}