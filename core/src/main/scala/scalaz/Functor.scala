package scalaz

////
/**
 * @see [[scalaz.Functor.FunctorLaw]]
 */
////
trait Functor[F[_]]  { self =>
  ////

  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived functions
  def apply[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def strengthL[A, B](a: A, f: F[B]): F[(A, B)] = map(f)(b => (a, b))

  def strengthR[A, B](f: F[A], b: B): F[(A, B)] = map(f)(a => (a, b))

  def mapply[A, B](a: A)(f: F[A => B]): F[B] = map(f)((ff: A => B) => ff(a))

  def fpair[A](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  def counzip[A, B](a: Either[F[A], F[B]]): F[Either[A, B]] =
    a match {
      case Left(x) => map(x)(Left(_))
      case Right(x) => map(x)(Right(_))
    }

  def counzipT[A, B](a: Either[F[A], F[B]]): EitherT[F, A, B] =
    error("") // EitherT(counzip(a))

  /**The composition of Functors `F` and `G`, `[x]F[G[x]]`, is a Functor */
  def compose[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = F[G[α]]})#λ] = new CompositionFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Functors `F` and `G`, `[x](F[x], G[x]])`, is a Functor */
  def product[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = (F[α], G[α])})#λ] = new ProductFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  trait FunctorLaw {
    def identity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(map(fa)(x => x), fa)
    def associative[A, B, C](fa: F[A], f1: A => B, f2: B => C)(implicit FC: Equal[F[C]]): Boolean = FC.equal(map(map(fa)(f1))(f2), map(fa)(f2 compose f1))
  }
  def functorLaw = new FunctorLaw {}
  ////
  val functorSyntax = new scalaz.syntax.FunctorSyntax[F] {}
}

object Functor {
  @inline def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  ////

  ////
}

