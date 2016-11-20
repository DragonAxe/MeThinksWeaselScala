import scala.util.Random

object Main extends App {
  val weasel = "METHINKS IT IS LIKE A WEASEL"
  println(weasel)
  val rnd = new Random()
  def loop(fittest: String, rnd: Random): Unit = {
    println(fittest)
    if (fittest != weasel) {
      // gen permutations
      val perm1 = Helpers.genPermutation(fittest, weasel)(rnd)
      val perm2 = Helpers.genPermutation(fittest, weasel)(perm1._2)
      val perm3 = Helpers.genPermutation(fittest, weasel)(perm2._2)
      val perm4 = Helpers.genPermutation(fittest, weasel)(perm3._2)
      val perm5 = Helpers.genPermutation(fittest, weasel)(perm4._2)
      val perm6 = Helpers.genPermutation(fittest, weasel)(perm5._2)
      val perm7 = Helpers.genPermutation(fittest, weasel)(perm6._2)
      val perm8 = Helpers.genPermutation(fittest, weasel)(perm7._2)
      val perm9 = Helpers.genPermutation(fittest, weasel)(perm8._2)
      val perm10 = Helpers.genPermutation(fittest, weasel)(perm9._2)
      val strings = List(perm1._1,perm2._1,perm3._1,perm4._1,perm5._1,perm6._1,perm7._1,perm8._1,perm9._1,perm10._1)
      // find fittest
      val newFittest = Helpers.getFittest(strings, weasel)
      // loop again
      loop(newFittest, perm9._2)
    }
  }
  val mRndStr = Helpers.genRandString(weasel.length)(rnd)
  loop(mRndStr._1, mRndStr._2)
}

object Helpers {
  def genRandInt(range: Int): Random => (Int, Random) = rnd => (rnd.nextInt(range), rnd)

  def genRandLetter: Random => (Char,Random) = rnd => {
    val mI = genRandInt(27)(rnd)
    val chr = (mI._1+'@').toChar
    (if (chr == '@') ' ' else chr, mI._2)
  }

  def genRandString(length: Int): Random => (String,Random) = rnd => {
    def loop(rnd: Random, str: String, length: Int): (String,Random) = {
      if (length > 0) {
        val mChr = genRandLetter(rnd)
        loop(mChr._2, mChr._1 + str, length - 1)
      } else {
        (str, rnd)
      }
    }
    loop(rnd, "", length)
  }

  def genPermutation(str: String, weasel: String): Random => (String,Random) = rnd => {
    def loop(rnd: Random, newStr: String, length: Int): (String,Random) = {
      if (length >= 0) {
        if (str.charAt(length) != weasel.charAt(length)) {
          val mChr = genRandLetter(rnd)
          loop(mChr._2, mChr._1 + newStr, length - 1)
        } else {
          loop(rnd, str.charAt(length) + newStr, length - 1)
        }
      } else {
        (newStr, rnd)
      }
    }
    loop(rnd, "", str.length-1)
  }

  def mutCmp(mut: String, weasel: String): Int = {
    def loop(index: Int, sum: Int): Int = {
      if (index < mut.length) {
        loop(index+1, if (mut.charAt(index)==weasel.charAt(index)) sum+1 else sum)
      } else {
        sum
      }
    }
    loop(0, 0)
  }

  def getFittest(strings: List[String], weasel: String): String = {
    def loop(rest: List[String], bestScore: Int, fittest: String): String = rest match {
      case x::xs => {
        val score = mutCmp(x, weasel)
        if (score > bestScore) {
          loop(xs, score, x)
        } else {
          loop(xs, bestScore, fittest)
        }
      }
      case Nil => fittest
    }
    loop(strings.tail, 0, strings.head)
  }
}


//trait M[A] {
//  def flatMap[B](f: A => M[B]): M[B] = ???
//  def map[B](g: A => B): M[B] = flatMap(x => unit(g(x)))
//}
//
//def unit[A](x: A): M[A] = ???

//case class M[S,A](exec: S => (S,A))
//object M {
//  def flatMap() = ???
//}

//type State[S,+A] = S => (S,A)
//type Rand[A] = State[Random, A]
//case object Rand {
//  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = state => {
//    val stf = f(state)
//    g(stf._2)(stf._1)
//  }
//  def map[A,B](f: Rand[A])(g: A => B): Rand[B] = state => {
//    val stf = f(state)
//    (stf._1, g(stf._2))
//  }
//}

//trait StateMonad[+T, S]  {
//  owner =>
//  def apply(state: S): (T, S)
//
//  def flatMap[U](f: T => StateMonad[U,S]) = new StateMonad[U, S] {
//    override def apply(state: S) = {
//      val (a, y) =  owner(state)
//      f(a)(y)
//    }
//  }
//
//  def map[U](f: T => U) = new StateMonad[U, S] {
//    def apply(state: S) = {
//      val (a, y) =  owner(state)
//      (f(a), y)
//    }
//  }
//}
//
//object StateMonad {
//  def apply[T, S](value: T) = new StateMonad[T, S] {
//    def apply(state: S) = (value, state)
//  }
//}
