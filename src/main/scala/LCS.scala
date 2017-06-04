/**
  * Created by yangshuxuan on 17-6-4.
  */
case class commonChar(i:Int,j:Int,s:Char)
object LCS {
  def findLCS(first:String,second:String): List[commonChar] ={
    val cacheLCS = collection.mutable.Map.empty[(Int,Int),List[commonChar]]
    def findLCSHelp(i:Int,j:Int):List[commonChar]={
      if(cacheLCS.contains((i,j)))
        cacheLCS((i,j))
      else {
        val t = if (i == -1 || j == -1) List.empty[commonChar]
        else if (first(i) == second(j)) commonChar(i, j, first(i)) :: findLCSHelp(i - 1, j - 1)
        else {
          val p = findLCSHelp(i - 1, j)
          val q = findLCSHelp(i, j - 1)
          if (p.length >= q.length) p else q
        }
        cacheLCS += (i,j) -> t
        t
      }
    }
    findLCSHelp(first.size-1,second.size-1)
  }

  def main(args: Array[String]): Unit = {
    val first="abcdef"
    val second = "bfdacdfa"
    val show = findLCS(first,second).map {case commonChar(_,_,c) => c} .reverse.mkString("->")
    println(show)
  }

}
