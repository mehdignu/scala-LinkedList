package list

case class Cons (val head:Int, val tail:IntList) extends IntList{

  def isEmpty=false
  
  def nth(index:Int):Int= index match{
    case 0 => head
    case i => tail.nth(i-1)
  }
  
  def contains(elem:Int):Boolean= elem match{
    case y if (y==head) => true
    case _ => tail.contains(elem)
  }

  def insert(X:Int):IntList= new Cons(X,this)
  	
  def insertSorted(elem:Int):IntList =  {
    if(isEmpty) new Cons(elem, this)
    else if(head <= elem){
      new Cons(head, insertSorted(elem)) else new Cons(elem, this)
    }

  }

  def delete(elem:Int):IntList= {
    if(head == elem) tail
    else Cons(head, tail.delete(elem))
  }

  def deleteAll(elem:Int):IntList = ???
    
  def insertionSortI:IntList= ???    
}