package patmat

abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman extends HuffmanInterface:

  def weight(tree: CodeTree): Int = tree match
    case Fork(left, right, chars, weight) => weight //     case Fork(_, _, _, weight) => weight
    case Leaf(char, weight) => weight //     case Leaf(_, weight) => weight

  def chars(tree: CodeTree): List[Char] = tree match
    case Fork(left, right, chars, weight) => chars //  case Fork(_, _, chars, _) => chars
    case Leaf(char, weight) => List(char) //    case Leaf(char, _) => List(char)

  private def makeCodeTree(left: CodeTree, right: CodeTree): Fork =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = // calculates the frequency
    var charsMap: Map[Char, Int] = Map()
    for (char <- chars)
      if charsMap.contains(char) then
        val count = charsMap.getOrElse(char, 0) + 1
        //        println(count)
        charsMap += (char -> count)
      else
        charsMap += (char -> 1)
    charsMap.toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = // generates Leafs from pairs
    freqs.sortBy(_._2).map(leaf => Leaf(leaf._1, leaf._2))

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1 // trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] =
  // if trees.size < 2 then trees
    if singleton(trees) then trees
    else makeCodeTree(trees.head, trees.tail.head) :: trees.tail.tail.sortBy(fork => weight(fork)) //sortWith ???
  // makes fork from two first leafs                 // makes one general fork

  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
    if done(trees) then trees
    else until(done, merge)(trees)

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  // chars-list --> pairs-list --> leaf-list --> combine

  private type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
    def decoding(rest: CodeTree, bits: List[Bit]): List[Char] =
    //      println(rest)
    //      println(bits)
      rest match
        case Leaf(char, _) => if bits.isEmpty then List(char) else char :: decoding(tree, bits) // rest??
        case Fork(left, right, _, _) => if bits.head == 1 then decoding(right, bits.tail) else decoding(left, bits.tail)

    decoding(tree, bits)


  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    def encoding(tree: CodeTree)(char: Char): List[Bit] =
    //      println(tree)
    //      println(char)
      tree match
        case Leaf(_, _) => List()
        case Fork(left, right, _, _) => if chars(left).contains(char) then 0 :: encoding(left)(char) else 1 :: encoding(right)(char)
    //    text.map(encoding(tree)).flatten
    // ::: ?
    text.flatMap(encoding(tree))


  private type CodeTable = List[(Char, List[Bit])]
  type Code = (Char, List[Bit])

  private def codeBits(table: CodeTable)(char: Char): List[Bit] =
    table.find(codeTable => codeTable._1 == char).head._2

  def convert(tree: CodeTree): CodeTable =
  //    println(tree)
    tree match
      case Leaf(char, _) => List((char, List()))
      case Fork(left, right, _, _) =>
        // println(mergeCodeTables(convert(left), convert(right)))
        mergeCodeTables(convert(left), convert(right))

  private def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable =
    //    println(a)
    //    println(b)
    //    println(1)
    //        a ::: b
    //    a ++ b
    //    a.concat(b)

    def concat(bit: Bit)(codeTable: CodeTable): CodeTable =
    //      println(codeTable)
    //      println(bit)
      codeTable.map(table =>
        //        println(table._1)
        //        println(table._2)
        (table._1, bit :: table._2))

    concat(0)(a) ::: concat(1)(b)


  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(codeBits(convert(tree)))

object Huffman extends Huffman


val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
@main def dec() = println(Huffman.decode(t2, List(0, 0, 0, 1, 1)))
@main def enc() = println(Huffman.encode(t2)("abd".toList))
@main def quickEnc() = println(Huffman.quickEncode(t2)("abd".toList))