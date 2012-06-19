package scalaz

sealed trait \/[+A, +B] {
  def isLeft: Boolean =
    this match {
      case -\/(_) => true
      case \/-(_) => false
    }

  def isRight: Boolean =
    this match {
      case -\/(_) => false
      case \/-(_) => true
    }

  def fold[X](l: A => X, r: B => X): X =
    this match {
      case -\/(a) => l(a)
      case \/-(b) => r(b)
    }

  def swap: (B \/ A) =
    this match {
      case -\/(a) => \/-(a)
      case \/-(b) => -\/(b)
    }

  def left: (A \\/ B) =
    new (A \\/ B) {
      val right = \/.this
    }

  def unary_- : (A \\/ B) =
    left

  def bimap[C, D](f: A => C, g: B => D): (C \/ D) =
    this match {
      case -\/(a) => -\/(f(a))
      case \/-(b) => \/-(g(b))
    }

  def map[D](g: B => D): (A \/ D) =
    bimap(identity, g)

  def foreach(g: B => Unit): Unit =
    bimap(_ => (), g)

  def flatMap[AA >: A, D](g: B => (AA \/ D)): (AA \/ D) =
    this match {
      case -\/(a) => -\/(a)
      case \/-(b) => g(b)
    }

  def filter[BB >: B](p: BB => Boolean)(implicit M: Monoid[BB]): (A \/ BB) =
    this match {
      case -\/(a) => -\/(a)
      case \/-(b) => \/-(if(p(b)) b else M.zero)
    }

  def exists[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => false
      case \/-(b) => p(b)
    }

  def forall[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => true
      case \/-(b) => p(b)
    }

  def toList: List[B] =
    this match {
      case -\/(_) => Nil
      case \/-(b) => List(b)
    }

  def toStream: Stream[B] =
    this match {
      case -\/(_) => Stream()
      case \/-(b) => Stream(b)
    }

  def toOption: Option[B] =
    this match {
      case -\/(_) => None
      case \/-(b) => Some(b)
    }

  def getOrElse[BB >: B](x: => BB): BB =
    toOption getOrElse x

  def ?[BB >: B](x: => BB): BB =
    getOrElse(x)

  def valueOr[BB >: B](x: A => BB): BB =
    this match {
      case -\/(a) => x(a)
      case \/-(b) => b
    }

  def orElse[AA >: A, BB >: B](x: => AA \/ BB): AA \/ BB =
    this match {
      case -\/(a) => x
      case \/-(_) => this
    }

  def |[AA >: A, BB >: B](x: => AA \/ BB): AA \/ BB =
    orElse(x)

  def ++[AA >: A, BB >: B](x: => AA \/ BB)(implicit M: Monoid[B]): AA \/ BB =
    this match {
      case -\/(a) => x
      case \/-(b) => this
    }
}
case class -\/[+A](a: A) extends (A \/ Nothing)
case class \/-[+B](b: B) extends (Nothing \/ B)

object \/ {
  // equal, order, semigroup, monoid, show
  // monad, cozip, traverse
  // bifunctor, bitraverse
}

sealed trait \\/[+A, +B] {
  val right: (A \/ B)

  def isLeft: Boolean =
    right.isLeft

  def isRight: Boolean =
    right.isRight

  def fold[X](l: A => X, r: B => X): X =
    right.fold(l, r)

  def swap: (B \\/ A) =
    right.swap.left

  def left: (A \\/ B) =
    this

  def unary_- : (A \\/ B) =
    left

  def bimap[C, D](f: A => C, g: B => D): (C \\/ D) =
    right.bimap(f, g).left

  def map[C](f: A => C): (C \/ B) =
    right.bimap(f, identity)

  def foreach(f: A => Unit): Unit =
    right.bimap(f, _ => ())

  def flatMap[BB >: B, C](f: A => (C \\/ BB)): (C \\/ BB) =
    right match {
      case -\/(a) => f(a)
      case \/-(b) => \/-(b).left
    }

  def filter[AA >: A](p: AA => Boolean)(implicit M: Monoid[AA]): (AA \\/ B) =
    (right match {
      case -\/(a) => -\/(if(p(a)) a else M.zero)
      case \/-(b) => \/-(b)
    }).left

  def exists[AA >: A](p: AA => Boolean): Boolean =
    right match {
      case -\/(a) => p(a)
      case \/-(_) => false
    }

  def forall[AA >: A](p: AA => Boolean): Boolean =
    right match {
      case -\/(a) => p(a)
      case \/-(_) => true
    }

  def toList: List[A] =
    right match {
      case -\/(a) => List(a)
      case \/-(_) => Nil
    }

  def toStream: Stream[A] =
    right match {
      case -\/(a) => Stream(a)
      case \/-(_) => Stream()
    }

  def toOption: Option[A] =
    right match {
      case -\/(a) => Some(a)
      case \/-(_) => None
    }

  def getOrElse[AA >: A](x: => AA): AA =
    toOption getOrElse x

  def ?[AA >: A](x: => AA): AA =
    getOrElse(x)

  def valueOr[AA >: A](x: B => AA): AA =
    right match {
      case -\/(a) => a
      case \/-(b) => x(b)
    }
}
