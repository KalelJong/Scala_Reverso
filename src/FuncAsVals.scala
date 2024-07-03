

object FuncAsVals extends App {

 var caca1 = List("pipi", "bumbum");

 def dosmth(): List[List[Boolean]] = {
   List.fill(8)(List.fill(8)(true))
 }

 caca1.foreach(println(_))
 dosmth().foreach(println(_))

 /* def makeBoard(rowsAndCols: Int, originalList: List[List[Option[Boolean]]]):List[List[Option[Boolean]]]   ={
  if(originalList.size == rowsAndCols){
   return originalList
  }
 var caca  =  originalList.add(djfalsdjflk;)
  makeBoard(rowsAndCols, caca)
  var i = rowsAndCols
  if()
  makeBoard()

 }*/

}
