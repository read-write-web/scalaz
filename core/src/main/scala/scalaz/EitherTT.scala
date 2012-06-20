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
sealed trait EitherTT[F[+_], +A, +B] {
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

  def swap(implicit F: Functor[F]): F[B \/ A] =
    mapR(_.swap)

  // todo left

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherTT[F, C, D] =
    EitherTT(mapR(_.bimap(f, g)))

  def bitraverse[G[+_], C, D](f: A => G[C], g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[EitherTT[F, C, D]] =
    G.map(F.traverse(run)(_.bitraverse(f, g)))(EitherTT(_))

  def map[D](f: B => D)(implicit F: Functor[F]): EitherTT[F, A, D] =
    EitherTT(mapR(_ map f))

  def traverse[G[+_], D](g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[EitherTT[F, A, D]] =
    G.map(F.traverse(run)(_.traverse(g)))(EitherTT(_))

  def foreach(g: B => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach g)

  def flatMap[AA >: A, D](g: B => EitherTT[F, AA, D])(implicit F: Monad[F]): EitherTT[F, AA, D] =
    EitherTT(bindR {
      case -\/(a) => F.point(-\/(a))
      case \/-(b) => g(b).run
    })

  def filter[AA >: A, BB >: B](p: BB => Boolean)(implicit M: Monoid[BB], F: Functor[F]): EitherTT[F, AA, BB] =
    EitherTT(mapR(_ filter p))

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

  def getOrElse[BB >: B](x: => BB)(implicit F: Functor[F]): F[BB] =
    mapR(_ getOrElse x)

  def ?[BB >: B](x: => BB)(implicit F: Functor[F]): F[BB] =
    getOrElse(x)

  def valueOr[BB >: B](x: A => F[BB])(implicit F: Monad[F]): F[BB] =
    bindR {
      case -\/(a) => x(a)
      case \/-(b) => F.point(b)
    }

  def orElse[AA >: A, BB >: B](x: => EitherTT[F, AA, BB])(implicit F: Monad[F]): EitherTT[F, AA, BB] =
    EitherTT(bindR(z => z match {
      case -\/(_) => x.run
      case \/-(_) => F.point(z)
    }))

  def |[AA >: A, BB >: B](x: => EitherTT[F, AA, BB])(implicit F: Monad[F]): EitherTT[F, AA, BB] =
    orElse(x)

  def ++[AA >: A, BB >: B](x: => EitherTT[F, AA, BB])(implicit M: Semigroup[BB], F: Monad[F]): EitherTT[F, AA, BB] =
    EitherTT(bindR(z => z match {
      case -\/(_) => F.point(z)
      case \/-(b1) => F.map(x.run) {
        case -\/(a2) => -\/(a2)
        case \/-(b2) => \/-(M.append(b1, b2))
      }
    }))
}

object EitherTT extends EitherTTInstances {
  def apply[F[+_], A, B](x: F[A \/ B]): EitherTT[F, A, B] =
    new EitherTT[F, A, B] {
      val run = x
    }
}

trait EitherTTInstances extends EitherTTInstances0

trait EitherTTInstances0 extends EitherTTInstances1 {
  implicit def EitherTTTraverse[F[+_]: Traverse, A]: Traverse[({type λ[α] = EitherTT[F, A, α]})#λ] =
    new Traverse[({type λ[α] = EitherTT[F, A, α]})#λ] {
      def traverseImpl[G[+_]: Applicative, X, Y](fa: EitherTT[F, A, X])(f: X => G[Y]) =
        fa traverse f
    }

  implicit def EitherTTMonad[F[+_]: Monad, A]: Monad[({type λ[α] = EitherTT[F, A, α]})#λ] =
    new Monad[({type λ[α] = EitherTT[F, A, α]})#λ] {
      def bind[X, Y](fa: EitherTT[F, A, X])(f: X => EitherTT[F, A, Y]): EitherTT[F, A, Y] = fa flatMap f
      def point[X](x: => X) = EitherTT(implicitly[Monad[F]].point(\/-(x)))
    }
}

trait EitherTTInstances1 extends EitherTTInstances2 {
  implicit def EitherTTBitraverse[F[+_]: Traverse]: Bitraverse[({type λ[α, β] = EitherTT[F, α, β]})#λ] =
    new Bitraverse[({type λ[α, β] = EitherTT[F, α, β]})#λ] {
      def bitraverseImpl[G[+_] : Applicative, A, B, C, D](fab: EitherTT[F, A, B])
                                                    (f: A => G[C], g: B => G[D]): G[EitherTT[F, C, D]] =
        fab bitraverse (f ,g)
    }
}

trait EitherTTInstances2 {
  implicit def EitherTTMonadTrans[A]: MonadTrans[({type λ[α[+_], β] = EitherTT[α, A, β]})#λ] =
    new MonadTrans[({type λ[α[+_], β] = EitherTT[α, A, β]})#λ] {
      def hoist[F[+_], G[+_]](f: F ~> G)(implicit M: Monad[F]) = new (({type λ[α] = EitherTT[F, A, α]})#λ ~> ({type λ[α] = EitherTT[G, A, α]})#λ) {
        def apply[B](mb: EitherTT[F, A, B]): EitherTT[G, A, B] = EitherTT(f.apply(mb.run))
      }
      def liftM[F[+_], B](mb: F[B])(implicit M: Monad[F]): EitherTT[F, A, B] = EitherTT(M.map(mb)(\/-(_)))
      implicit def apply[F[+_] : Monad]: Monad[({type λ[α] = EitherTT[F, A, α]})#λ] =
        new Monad[({type λ[α] = EitherTT[F, A, α]})#λ] {
          def bind[X, Y](fa: EitherTT[F, A, X])(f: X => EitherTT[F, A, Y]): EitherTT[F, A, Y] = fa flatMap f
          def point[X](x: => X) = EitherTT(implicitly[Monad[F]].point(\/-(x)))
        }
    }
}
