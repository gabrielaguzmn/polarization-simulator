import Opinion._

val frequencies1 = Vector(0.5, 0.5)
val distributionValues1 = Vector(0.0, 1.0)
val distribution1 = (frequencies1, distributionValues1)

val frequencies2 = Vector(0.05, 0.1, 0.15, 0.2, 0.25, 0.25)
val distributionValues2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
val distribution2 = (frequencies2, distributionValues2)

val frequencies3 = Vector(0.7, 0.2, 0.1)
val distributionValues3 = Vector(0.0, 0.5, 1.0)
val distribution3 = (frequencies3, distributionValues3)

val frequencies4 = Vector(0.2, 0.2, 0.2, 0.2, 0.2)
val distributionValues4 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val distribution4 = (frequencies4, distributionValues4)

val frequencies5 = Vector(0.1, 0.4, 0.4, 0.1)
val distributionValues5 = Vector(0.0, 0.3, 0.7, 1.0)
val distribution5 = (frequencies5, distributionValues5)

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

val sb_ext= allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief (100)
val sb_midly = midlyBelief(100)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0 ,1.0)
val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)

rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)

rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)

rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief(100)
val sb_midly = midlyBelief(100)
val rhoPar1 = rhoPar(1.2,1.2)
val rhoPar2 = rhoPar(2.0,1.0)
val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
rhoPar1(sb_ext, dist1)
rhoPar2(sb_ext, dist1)
rhoPar1(sb_ext, dist2)
rhoPar2(sb_ext, dist2)
rhoPar1(sb_cons, dist1)
rhoPar2(sb_cons, dist1)
rhoPar1(sb_cons, dist2)
rhoPar2(sb_cons, dist2)
rhoPar1(sb_unif, dist1)
rhoPar2(sb_unif, dist1)
rhoPar1(sb_unif, dist2)
rhoPar2(sb_unif, dist2)
rhoPar1(sb_triple, dist1)
rhoPar2(sb_triple, dist1)
rhoPar1(sb_triple, dist2)
rhoPar2(sb_triple, dist2)
rhoPar1(sb_midly, dist1)
rhoPar2(sb_midly, dist1)
rhoPar1(sb_midly, dist2)
rhoPar2(sb_midly, dist2)
val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)
val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)
confBiasUpdatePar(sbu_10, i1_10)
rhoPar1(sbu_10, dist1)
rhoPar1(confBiasUpdatePar(sbu_10, i1_10), dist1)

confBiasUpdatePar(sbm_10, i1_10)
rhoPar1(sbm_10, dist1)
rhoPar1(confBiasUpdatePar(sbm_10, i1_10), dist1)


