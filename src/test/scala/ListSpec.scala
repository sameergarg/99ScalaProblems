import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {

  //`p01 Find the last element of a list`
  def last[A](list: List[A]): Option[A] = list match {
    case Nil       => None
    case h :: Nil  => Some(h)
    case _ :: tail => last(tail)
  }

  //P02 (*) Find the last but one element of a list.
  def penultimate[A](l: List[A]): Option[A] = l match {
    case Nil           => None
    case h :: _ :: Nil => Some(h)
    case _ :: tail     => penultimate(tail)
  }

  //P08 (**) Eliminate consecutive duplicates of list elements.
  def compress(l: List[Symbol]): List[Symbol] = l match {
    case Nil       => Nil
    case h :: tail => h :: compress(tail.dropWhile(_ == h))
  }

  //P09 (**) Pack consecutive duplicates of list elements into sublists
  def pack(l: List[Symbol]): List[List[Symbol]] = l match {
    case Nil => Nil
    case h :: tail =>
      val (dups, rest) = tail.span(_ == h)
      (h :: dups) :: pack(rest)
  }

  //P10 (*) Run-length encoding of a list
  def encode(l: List[Symbol]): List[(Int, Symbol)] =
    pack(l).iterator.map { dups =>
      (dups.length, dups.head)
    }.toList

  //P16 (**) Drop every Nth element from a list
  def drop(n: Int, l: List[Symbol]): List[Symbol] =
    l.zipWithIndex.filter { case (_, i) => (i + 1) % n != 0 }.map(_._1)

  //P18 (**) Extract a slice from a list

  def slice(s: Int, e: Int, l: List[Symbol]): List[Symbol] =
    l.zipWithIndex
      .filter {
        case (_, i) => i >= s && i < e
      }
      .map(_._1)

  //P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list

  def combinations(n: Int, l: List[Symbol]): List[List[Symbol]] = {
    //l.combinations(n).toList
    (n, l) match {
      case (0, _)                  => List(Nil)
      case (_, Nil)                => Nil
      case (n, l) if (l.size <= n) => List(l)
      case (n, h :: tail) =>
        combinations(n - 1, tail).map(h :: _) ::: combinations(n, tail)
    }
  }

  "In list" should {

    "find last element" in {
      last(List(1, 2, 3, 4, 5)).value shouldBe 5
    }

    "Find the last but one element of a list" in {
      penultimate(List(1, 1, 2, 3, 5, 8)).value shouldBe 5
    }

    "Eliminate consecutive duplicates of list elements" in {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(
          'a, 'b, 'c, 'a, 'd, 'e)
    }

    "P09 (**) Pack consecutive duplicates of list elements into sublists" in {
      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe
        List(
          List('a, 'a, 'a, 'a),
          List('b),
          List('c, 'c),
          List('a, 'a),
          List('d),
          List('e, 'e, 'e, 'e)
        )
    }

    "P10 (*) Run-length encoding of a list." in {
      encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(
        (4, 'a),
        (1, 'b),
        (2, 'c),
        (2, 'a),
        (1, 'd),
        (4, 'e)
      )
    }

    "P16 (**) Drop every Nth element from a list" in {
      drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List(
          'a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    }

    "P18 (**) Extract a slice from a list" in {
      slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List(
        'd,
        'e,
        'f,
        'g
      )
    }

    "P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list." in {
      combinations(2, List('a, 'b, 'c)) should contain theSameElementsAs Vector(
        List('a, 'b),
        List('b, 'c),
        List('a, 'c)
      )
    }

  }
}
