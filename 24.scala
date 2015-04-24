var p = List(0)
val perms2 = List(0,1,2,3,4,5,6,7,8,9).permutations
(1 to 1000000).foreach { i => p = perms2.next() }
println(p.mkString(""))
