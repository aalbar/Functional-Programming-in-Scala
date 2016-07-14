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


def until(condition: List[CodeTree] => Boolean,
          action: List[CodeTree] => List[CodeTree])
         (trees: List[CodeTree]): List[CodeTree] =
  if (condition(trees))
    trees
  else
    until(condition, action)(action(trees))


