import Opinion._
def i1(nags:Int):SpecificWeightedGraph={
  ((i:Int,j:Int) => if (i==j) 1.0
  else if (i<j) 1.0/(j-i).toDouble
  else 0.0,nags)
}
def uniformBelief(nags: Int):SpecificBelief={
  val middle = nags/2
  Vector.tabulate(nags)((i:Int)=>
    if (i<middle)math.max(0.25-0.01*(middle-i-1),0)
    else math.min(0.75-0.01*(middle-i),1))
}
val rho1= rho(1.2,1.2)
val i1_10=i1(10)
val sbu_10= uniformBelief(10)
val dist1= Vector(0.0,0.25,0.50,0.75,1.0)
for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2 )
} yield (b,rho1(b,dist1))