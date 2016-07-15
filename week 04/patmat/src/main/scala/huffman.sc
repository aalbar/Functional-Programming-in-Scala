import common._
import patmat.Huffman._

//val charList = List('a', 'b', 'a', 'c', 'c', 'a', 'd', 'b')
//
//def times(chars: List[Char]): List[(Char, Int)] =
//  chars.groupBy(x => x).map(x => (x._1, x._2.size)).toList
//
//
//val sample = times(charList)
//
//def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
//  freqs.sortWith(_._2 < _._2) map (x => Leaf(x._1, x._2))
//
//val orderedLeaves = makeOrderedLeafList(sample)
//
//def combine(trees: List[CodeTree]): List[CodeTree] =
//  if (trees.size < 2) trees
//  else (makeCodeTree(trees.head, trees(1)) +: trees.drop(2)).sortWith(weight(_) < weight(_))
//
//val tree = combine(orderedLeaves)
//
//
//def until(condition: List[CodeTree] => Boolean,
//          action: List[CodeTree] => List[CodeTree])
//         (trees: List[CodeTree]): List[CodeTree] =
//  if (condition(trees))
//    trees
//  else
//    until(condition, action)(action(trees))

//===================================

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decoder(subTree: CodeTree, remainingBits: List[Bit], accumulated: List[Char]): List[Char] = {
    (subTree, remainingBits) match {
      case (subTree: Leaf, Nil)       => accumulated :+ subTree.char
      case (subTree: Leaf, rest)      => decoder(tree, rest, accumulated :+ subTree.char)
      case (subTree: Fork, 0 :: rest) => decoder(subTree.left, rest, accumulated)
      case (subTree: Fork, 1 :: rest) => decoder(subTree.right, rest, accumulated)
      case (_, _) => Nil
    }
  }
  decoder(tree, bits, List())
}


def decodedSecret: List[Char] = decode(frenchCode, secret)


