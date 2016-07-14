import common._
import patmat.Huffman._

val charList = List('a', 'b', 'a', 'c', 'c', 'a', 'd', 'b')

def times(chars: List[Char]): List[(Char, Int)] =
  chars.groupBy(x => x).map(x => (x._1, x._2.size)).toList


val sample = times(charList)

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqs.sortWith(_._2 < _._2) map (x => Leaf(x._1, x._2))

val orderedLeaves = makeOrderedLeafList(sample)

def combine(trees: List[CodeTree]): List[CodeTree] =
  if (trees.size < 2) trees
  else (makeCodeTree(trees.head, trees(1)) +: trees.drop(2)).sortWith(weight(_) < weight(_))

val tree = combine(orderedLeaves)


/**
  * This function will be called in the following way:
  *
  *   until(singleton, combine)(trees)
  *
  * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
  * the two functions defined above.
  *
  * In such an invocation, `until` should call the two functions until the list of
  * code trees contains only one single tree, and then return that singleton list.
  *
  * Hint: before writing the implementation,
  *  - start by defining the parameter types such that the above example invocation
  *    is valid. The parameter types of `until` should match the argument types of
  *    the example invocation. Also define the return type of the `until` function.
  *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
  */
def until(xxx: ???, yyy: ???)(zzz: ???): ??? = ???


