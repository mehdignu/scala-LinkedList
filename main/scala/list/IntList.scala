package list

abstract class IntList {

  def isEmpty:Boolean
  def head:Int
  def tail:IntList
  def nth(index:Int):Int
  def contains(elem:Int):Boolean
  def insert(elem:Int):IntList
  def insertSorted(elem:Int):IntList

  def delete(elem: Int): IntList

//  def delete(elem:Int):IntList = this match {
//    case Empty => Empty
//    case Cons(head, tail) => if(head == elem) tail else Cons(head, tail.delete(elem))
//  }

  def deleteAll(elem:Int):IntList
	def insertionSortI:IntList
	
  def insertSO(elem:Int):IntList= ???
  
  def insertionSort:IntList= ???
  
  def prefix(l:IntList):IntList= ??? 
  
  def flip:IntList= ???
}