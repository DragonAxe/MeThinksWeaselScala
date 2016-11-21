import scala.util.Random

object Main extends App {
  val weasel = "METHINKS IT IS LIKE A WEASEL"
  println(weasel)
  val rnd = new Random()//1
  def loop(fittest: String, rnd: Random, count: Int): Unit = {
    println(count+": "+fittest)
    if (fittest != weasel) {
      // gen permutations
      def permGen(rnd: Random, n: Int, perms: List[String]): (List[String], Random) = {
        if (n >= 0) {
          val perm = Helpers.genPermutation(fittest)(rnd)
          permGen(perm._2, n-1, perm._1::perms)
        } else {
          (perms,rnd)
        }
      }
      val strings = permGen(rnd, 10, Nil)
      // find fittest
      val newFittest = Helpers.getFittest(strings._1, weasel)
      // loop again
      loop(newFittest, strings._2, count+1)
    }
  }
  val mRndStr = Helpers.genRandString(weasel.length)(rnd)
  loop(mRndStr._1, mRndStr._2, 0)
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

  def genPermutation(str: String): Random => (String,Random) = rnd => {
    def loop(rnd: Random, newStr: String, length: Int): (String,Random) = {
      if (length >= 0) {
        val mWeight = genRandInt(30)(rnd)
        if (mWeight._1 == 0) {
          val mChr = genRandLetter(mWeight._2)
          loop(mChr._2, mChr._1 + newStr, length - 1)
        } else {
          loop(mWeight._2, str.charAt(length) + newStr, length - 1)
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
