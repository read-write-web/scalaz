package scalaz

/**
 * Represents a computation of type `F[Either[A, B]]`.
 *
 * Example:
 * {{{
 * val x: Option[Either[String, Int]] = Some(Right(1))
 * EitherT(x).map(1+).run // Some(Right(2)
 * }}}
 *
 */
sealed trait EitherT[F[+_], +A, +B] {
  val run: F[A \/ B]

  private def mapR[X](f: A \/ B => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(f)

  private def bindR[X](f: A \/ B => F[X])(implicit F: Bind[F]): F[X] =
    F.bind(run)(f)

  def isLeft(implicit F: Functor[F]): F[Boolean] =
    mapR(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] =
    mapR(_.isRight)

  def fold[X](l: A => F[X], r: B => F[X])(implicit F: Bind[F]): F[X] =
    bindR(_.fold(l, r))

  def swap(implicit F: Functor[F]): EitherT[F, B, A] =
    EitherT(mapR(_.swap))

  def left: EitherTLeft[F, A, B] =
    new EitherTLeft[F, A, B] {
      val right = EitherT.this
    }

  def unary_- : EitherTLeft[F, A, B] =
    left

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(mapR(_.bimap(f, g)))

  def bitraverse[G[+_], C, D](f: A => G[C], g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[EitherT[F, C, D]] =
    G.map(F.traverse(run)(_.bitraverse(f, g)))(EitherT(_))

  def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] =
    EitherT(mapR(_ map f))

  def traverse[G[+_], D](g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[EitherT[F, A, D]] =
    bitraverse(G.point(_), g)

  def foreach(g: B => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach g)

  def flatMap[AA >: A, D](g: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
    EitherT(bindR {
      case -\/(a) => F.point(-\/(a))
      case \/-(b) => g(b).run
    })

  def filter[AA >: A, BB >: B](p: BB => Boolean)(implicit M: Monoid[BB], F: Functor[F]): EitherT[F, AA, BB] =
    EitherT(mapR(_ filter p))

  def exists[BB >: B](p: BB => Boolean)(implicit F: Functor[F]): F[Boolean] =
    mapR(_ exists p)

  def forall[BB >: B](p: BB => Boolean)(implicit F: Functor[F]): F[Boolean] =
    mapR(_ forall p)

  def toList(implicit F: Functor[F]): F[List[B]] =
    mapR(_.toList)

  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    mapR(_.toStream)

  def toOption(implicit F: Functor[F]): F[Option[B]] =
    mapR(_.toOption)

  def toOptionT[BB >: B](implicit F: Functor[F]): OptionT[F, BB] =
    OptionT(toOption)

  def toEither(implicit F: Functor[F]): F[Either[A, B]] =
    mapR(_.toEither)

  def getOrElse[BB >: B](x: => BB)(implicit F: Functor[F]): F[BB] =
    mapR(_ getOrElse x)

  def ?[BB >: B](x: => BB)(implicit F: Functor[F]): F[BB] =
    getOrElse(x)

  def valueOr[BB >: B](x: A => F[BB])(implicit F: Monad[F]): F[BB] =
    bindR {
      case -\/(a) => x(a)
      case \/-(b) => F.point(b)
    }

  def orElse[AA >: A, BB >: B](x: => EitherT[F, AA, BB])(implicit F: Monad[F]): EitherT[F, AA, BB] =
    EitherT(bindR(z => z match {
      case -\/(_) => x.run
      case \/-(_) => F.point(z)
    }))

  def |[AA >: A, BB >: B](x: => EitherT[F, AA, BB])(implicit F: Monad[F]): EitherT[F, AA, BB] =
    orElse(x)

  def ++[AA >: A, BB >: B](x: => EitherT[F, AA, BB])(implicit M: Semigroup[BB], F: Monad[F]): EitherT[F, AA, BB] =
    EitherT(bindR(z => z match {
      case -\/(_) => F.point(z)
      case \/-(b1) => F.map(x.run) {
        case -\/(a2) => -\/(a2)
        case \/-(b2) => \/-(M.append(b1, b2))
      }
    }))
}

object EitherT extends EitherTInstances {
  def apply[F[+_], A, B](x: F[A \/ B]): EitherT[F, A, B] =
    new EitherT[F, A, B] {
      val run = x
    }
}

trait EitherTInstances extends EitherTInstances0 {
  type \^/[+A, +B] =
  EitherT[Id.Id, A, B]

  type GlorifiedTupleT[F[+_], +A, +B] =
  EitherT[F, A, B]
}

trait EitherTInstances0 extends EitherTInstances1 {
  implicit def EitherTraverse[F[+_]: Traverse, A]: Traverse[({type λ[α] = EitherT[F, A, α]})#λ] =
    new Traverse[({type λ[α] = EitherT[F, A, α]})#λ] {
      def traverseImpl[G[+_]: Applicative, X, Y](fa: EitherT[F, A, X])(f: X => G[Y]) =
        fa traverse f
    }

  implicit def EitherTMonad[F[+_]: Monad, A]: Monad[({type λ[α] = EitherT[F, A, α]})#λ] =
    new Monad[({type λ[α] = EitherT[F, A, α]})#λ] {
      def bind[X, Y](fa: EitherT[F, A, X])(f: X => EitherT[F, A, Y]): EitherT[F, A, Y] = fa flatMap f
      def point[X](x: => X) = EitherT(implicitly[Monad[F]].point(\/-(x)))
    }
}

trait EitherTInstances1 extends EitherTInstances2 {
  implicit def EitherTBitraverse[F[+_]: Traverse]: Bitraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] =
    new Bitraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] {
      def bitraverseImpl[G[+_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                    (f: A => G[C], g: B => G[D]): G[EitherT[F, C, D]] =
        fab bitraverse (f ,g)
    }
}

trait EitherTInstances2 {
  implicit def EitherTMonadTrans[A]: MonadTrans[({type λ[α[+_], β] = EitherT[α, A, β]})#λ] =
    new MonadTrans[({type λ[α[+_], β] = EitherT[α, A, β]})#λ] {
      def hoist[F[+_], G[+_]](f: F ~> G)(implicit M: Monad[F]) = new (({type λ[α] = EitherT[F, A, α]})#λ ~> ({type λ[α] = EitherT[G, A, α]})#λ) {
        def apply[B](mb: EitherT[F, A, B]): EitherT[G, A, B] = EitherT(f.apply(mb.run))
      }
      def liftM[F[+_], B](mb: F[B])(implicit M: Monad[F]): EitherT[F, A, B] = EitherT(M.map(mb)(\/-(_)))
      implicit def apply[F[+_] : Monad]: Monad[({type λ[α] = EitherT[F, A, α]})#λ] =
        new Monad[({type λ[α] = EitherT[F, A, α]})#λ] {
          def bind[X, Y](fa: EitherT[F, A, X])(f: X => EitherT[F, A, Y]): EitherT[F, A, Y] = fa flatMap f
          def point[X](x: => X) = EitherT(implicitly[Monad[F]].point(\/-(x)))
        }
    }
}

sealed trait EitherTLeft[F[+_], +A, +B] {
  val right: EitherT[F, A, B]

  def run(implicit F: Functor[F]): F[A \\/ B] =
    F.map(right.run)(_.left)

  def isLeft(implicit F: Functor[F]): F[Boolean] =
    right.isLeft

  def isRight(implicit F: Functor[F]): F[Boolean] =
    right.isRight

  def fold[X](l: A => F[X], r: B => F[X])(implicit F: Bind[F]): F[X] =
    right.fold(l, r)

  def swap(implicit F: Functor[F]): EitherTLeft[F, B, A] =
    right.swap.left

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherTLeft[F, C, D] =
    right.bimap(f, g).left

  def bitraverse[G[+_], C, D](f: A => G[C], g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[EitherTLeft[F, C, D]] =
    G.map(F.traverse(right.run)(_.bitraverse(f, g)))(EitherT(_).left)

  def map[C](f: A => C)(implicit F: Functor[F]): EitherTLeft[F, C, B] =
    bimap(f, identity)

  def traverse[G[+_], C](f: A => G[C])(implicit G: Applicative[G], F: Traverse[F]): G[EitherTLeft[F, C, B]] =
    bitraverse(f, G.point(_))

  def foreach(f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(right.run)(_.left foreach f)

  def flatMap[BB >: B, C](g: A => EitherTLeft[F, C, BB])(implicit F: Monad[F]): EitherTLeft[F, C, BB] =
    EitherT(F.bind(right.run) {
      case -\/(a) => g(a).right.run
      case \/-(b) => F.point(\/-(b))
    }).left

  def filter[AA >: A, BB >: B](p: AA => Boolean)(implicit M: Monoid[AA], F: Functor[F]): EitherTLeft[F, AA, BB] =
    EitherT(F.map(right.run)(_.left filter p right)).left

  def exists[AA >: A, BB >: B](p: AA => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(right.run)(_.left exists p)

  def forall[AA >: A, BB >: B](p: AA => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(right.run)(_.left forall p)

  def toList(implicit F: Functor[F]): F[List[A]] =
    F.map(right.run)(_.left.toList)

  def toStream(implicit F: Functor[F]): F[Stream[A]] =
    F.map(right.run)(_.left.toStream)

  def toOption(implicit F: Functor[F]): F[Option[A]] =
    F.map(right.run)(_.left.toOption)

  def toOptionT[AA >: A](implicit F: Functor[F]): OptionT[F, AA] =
    OptionT(toOption)

  def toEither(implicit F: Functor[F]): F[Either.LeftProjection[A, B]] =
    F.map(right.run)(_.toEither.left)

  def getOrElse[AA >: A](x: => AA)(implicit F: Functor[F]): F[AA] =
    F.map(right.run)(_.left getOrElse x)

  def ?[AA >: A](x: => AA)(implicit F: Functor[F]): F[AA] =
    getOrElse(x)

  def valueOr[AA >: A](x: B => F[AA])(implicit F: Monad[F]): F[AA] =
    F.bind(right.run) {
      case -\/(a) => F.point(a)
      case \/-(b) => x(b)
    }

  def orElse[AA >: A, BB >: B](x: => EitherTLeft[F, AA, BB])(implicit F: Monad[F]): EitherTLeft[F, AA, BB] =
    EitherT(F.bind(right.run)(z => z match {
      case -\/(_) => F.point(z)
      case \/-(_) => x.right.run
    })).left

  def |[AA >: A, BB >: B](x: => EitherTLeft[F, AA, BB])(implicit F: Monad[F]): EitherTLeft[F, AA, BB] =
    orElse(x)

  def ++[AA >: A, BB >: B](x: => EitherTLeft[F, AA, BB])(implicit M: Semigroup[AA], F: Monad[F]): EitherTLeft[F, AA, BB] =
    EitherT(F.bind(right.run)(z => z match {
      case \/-(_) => F.point(z)
      case -\/(a1) => F.map(x.right.run) {
        case \/-(b2) => \/-(b2)
        case -\/(a2) => -\/(M.append(a1, a2))
      }
    })).left
}

