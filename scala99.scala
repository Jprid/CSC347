 object scala99{
 	// 1.
	 def last(xs: List[Any]) : Any = { 
	 	xs match {
	 		case y::Nil => y
	 		case y::ys  => last(ys)
	 		case _      => throw new NoSuchElementException
	 	}
	 }
	 // 2.
	 def penultimate [X] (xs: List[X]) : X = { 
	 	xs match {
	 		//case z::Nil    => 
	 		case y:: _ ::Nil => y
	 		case y::z::ys    => penultimate(ys)
	 		case _			 => throw new NoSuchElementException
 	 	}
	 }
	 // find the nth element in a list 
	 // nth(2, List(1,2,3,4,5))
	 // nth(1, List(2,3,4,5))
	 // nth(0, List(3,4,5))
	 // 3
	 // O(n)
	 
	 def nth [X] (n:Int, xs:List[X]) : X = {
	 	xs match{
	 		case Nil => throw new NoSuchElementException 
	 		case y::_ if (n == 0) => y
	 		case y::ys => nth(n-1, ys)
	 	}
	 }
	 // P04
	 // length
	 // find number of elements in list
	 // O(n)
	 def length [X] (xs:List[X]) : Int = {
	 	xs match{
	 		case Nil   => 0 
	 		case y::ys => 1 + length(ys)
	 	}
	 }

	 def lengthFunctional (xs : List[Int]) : Int = {
	 	xs.foldLeft(1) ((accum, y) => accum * y)
	 }

	 // PO5
	 // reverse a list
	 // bad implementation
	 // O(n^2)
	 def reverse [X] (xs: List[X]) : List[X] = {
	 	xs match{
	 		case Nil    => Nil
	 		case y::Nil => List(y)
	 		case y::ys  => reverse(ys):::List(y)
	 	}
	 }
	 // good implementation
	 def reverseTail [X] (xs: List[X]) : List[X] = {
	 	xs match {
	 		case Nil => Nil
	 		case _   => reverseTailAux(xs, Nil)
	 	}
	 }
	 // example of recursion logic
	 // reverseTailAux((1,2,3), Nil)
	 // reverseTailAux((2,3), 1::Nil)
 	 // reverseTailAux((3), 2::1::Nil)
 	 // reverseTailAux(Nil, 3::2::1::Nil)
 	 // O(N) (Tail recursive)
	 def reverseTailAux [X] (xs: List[X], accumulator:List[X]) : List[X] = {
	 	xs match{
	 		case Nil => accumulator
	 		case y::ys => reverseTailAux(ys, y::accumulator)
	 		// y::accumulator acts like stack, pushes ith index down onto list
	 	}
	 }

	// P06 (*) Find out whether a list is a palindrome.
	// Example:
	// scala> isPalindrome(List(1, 2, 3, 2, 1))
	// res0: Boolean = true
	// O(N + N)
	// O(N)

	def isListPalindrome [X] (xs: List[X]) : Boolean = {
		// empty set == empty set; reverse(empty set) == empty set
		xs == reverseTail(xs)
	}

	// P07 (**) Flatten a nested list structure.
	// Example:
	// scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
	// res0: List[Any] = List(1, 1, 2, 3, 5, 8)
	//
	// O(M) M == length of inner list = N * L (N == outer.length, L == inner.length)

	def flatten [X] (xs: List[List[X]]) : List[X] = {
		for (x <- xs; y <- x) yield y 
		//xs match{
		//	case Nil   => Nil
		//	case y::ys => y::flatten(ys)
		//}
	}

	// P08 (**) Eliminate consecutive duplicates of list elements.
	// If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
	// Example:
	// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
	//
	//	O()
	// compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// a' :: compress(List('a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	 
	def removeConsecDups [X] (xs: List[X]) : List[X] = {
		xs match{
			case Nil      			  => Nil
			case y::z::ys if (y == z) => removeConsecDups(y::ys)
			case y::ys 			      => y::removeConsecDups(ys)
		}
	}
	// 	P09 (**) Pack consecutive duplicates of list elements into sublists.
	// If a list contains repeated elements they should be placed in separate sublists.
	// Example:

	// scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))		
	// pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// List(List('a), 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
 	// List(List('a, 'a), 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))))
 	// O(N)

	def packConsecDups [X] (xs: List[X]) : List[List[X]] = {
		xs match {
			case Nil => Nil
			case ys:List[_] => {
				val (packed, next) = packAux(ys, Nil) // U * packAux where U == # unique characters
				packed :: packConsecDups(next) 		  // U * avglength(unique characters)
			}
		}
	}

	def packAux [X] (xs: List[X], accumulator : List[X]) : (List[X], List[X]) = {
		xs match {
			case Nil      => (accumulator, Nil)
			case y::z::ys if (y==z) => packAux(y::ys, y::accumulator)
			case y::Nil   => (y::accumulator, Nil)
			case y::z::ys => (y::accumulator, z::ys) // not equal
		}
	}
	// P10 (*) Run-length encoding of a list.
	// Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	// Example:
	//
	// scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	// O(N)
	def encode [X] (xs: List[X]) : List[(Int, X)] = {
		// alternative: pack(ls) map { e => (e.length, e.head) }
		xs match{
			case Nil        => Nil
			case ys:List[_] => {
				val (packed, next) = packAux(ys, Nil)
				(packed.length, packed.head) :: encode(next)
			} 
		}
	}
	// P11 (*) Modified run-length encoding.
	// Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
	// Example:
	// scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	// res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
	// O(N)
	def encodeModified [X] (xs: List[X]) : List[Any] = {
		xs match{
			case Nil        => Nil
			case ys:List[_] => {
				val (packed, next) = packAux(ys, Nil)
				if (packed.length == 1) packed.head :: encodeModified(next)
				else (packed.length, packed.head) :: encodeModified(next)
			} 

		}
	}	
	// P12 (**) Decode a run-length encoded list.
	// Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
	// Example:
	// scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	// res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	
	def decode [X] (xs: List[(Int, X)]) : List[X] = {
		// = ls flatMap { e => List.make(e._1, e._2) }
		xs match{
			case Nil 				=> Nil
			case (num:Int, e:X)::ys => {
				var result: List[X] = Nil
				for(i <- 1 to num) {
					result = e :: result
				}
				result ::: decode(ys) 
			}
		}
	}




}
// }