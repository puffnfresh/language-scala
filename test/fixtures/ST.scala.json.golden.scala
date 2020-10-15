package scalaz
package effect

import reflect.ClassTag

import STRef._

import STArray._

import ST._

import std.function._

import Id._

sealed abstract class STRef[S, A]  {
  protected var value: A

  def read: ST[S, A] =
    returnST(value)

  def mod[B](f: A => A): ST[S, STRef[S, A]] =
    st( () => {
      value = f(value)

      this
    } )

  def write(a: => A): ST[S, STRef[S, A]] =
    st( () => {
      value = a

      this
    } )

  def |=(a: => A): ST[S, STRef[S, A]] =
    write(a)

  def swap(that: STRef[S, A]): ST[S, Unit] =
    for {
      v1 <- this.read
      v2 <- that.read
      _ <- this write v2
      _ <- that write v1
    } yield ()
}

object STRef extends STRefInstances {
  def apply[S]: Id ~> STRef[S, *] =
    stRef[S]

  def stRef[S]: Id ~> STRef[S, *] =
    new (Id ~> STRef[S, *]) {
      def apply[A](a: A) =
        new STRef[S, A] {
          var value =
            a
        }
    }
}

sealed abstract class STRefInstances  {
  implicit def STRefEqual[S, A]: Equal[STRef[S, A]] =
    Equal.equalA
}

sealed abstract class STArray[S, A]  {
  def size: Int

  def z: A

  implicit def tag: ClassTag[A]

  private[this] val arr =
    Need(Array.fill(size)(z))

  private def value: Array[A] =
    arr.value

  import ST._

  def read(i: Int): ST[S, A] =
    returnST(value(i))

  def write(i: Int, a: A): ST[S, STArray[S, A]] =
    st( () => {
      value(i) = a

      this
    } )

  def freeze: ST[S, ImmutableArray[A]] =
    st(() => ImmutableArray.fromArray(value))

  def fill[B](f: (A, B) => A, xs: Iterable[(Int, B)]): ST[S, Unit] =
    xs.toList match {
      case Nil =>
        returnST(())
      case (i, v) :: (ivs) =>
        for {
          _ <- update(f, i, v)
          _ <- fill(f, ivs)
        } yield ()
    }

  def update[B](f: (A, B) => A, i: Int, v: B): ST[S, Unit] =
    for {
      x <- read(i)
      _ <- write(i, f(x, v))
    } yield ()
}

object STArray {
  def apply[S, A: ClassTag](s: Int, a: A): STArray[S, A] =
    stArray(s, a)

  def stArray[S, A](s: Int, a: A)(implicit t: ClassTag[A]): STArray[S, A] =
    new STArray[S, A] {
      val size =
        s

      val z =
        a

      implicit val tag =
        t
    }
}

sealed abstract class ST[S, A]  {
  private[effect] def run: A

  import ST._

  def flatMap[B](g: A => ST[S, B]): ST[S, B] =
    st(() => g(run).run)

  def map[B](g: A => B): ST[S, B] =
    st(() => g(run))
}

object ST extends STInstances {
  def apply[S, A](a: => A): ST[S, A] =
    returnST(a)

  def st[S, A](f: () => A): ST[S, A] =
    new ST[S, A] {
      private[effect] def run =
        f()
    }

  def STToIO[A](st: ST[IvoryTower, A]): IO[A] =
    IO.io((rw) => Free.return_((rw, st.run)))

  def returnST[S, A](a: => A): ST[S, A] =
    st(() => a)

  def runST[A](f: Forall[ST[*, A]]): A =
    f.apply.run

  def newVar[S]: Id ~> λ[α => ST[S, STRef[S, α]]] =
    new (Id ~> λ[α => ST[S, STRef[S, α]]]) {
      def apply[A](a: A) =
        returnST(stRef[S](a))
    }

  def newArr[S, A: ClassTag](size: Int, z: A): ST[S, STArray[S, A]] =
    returnST(stArray[S, A](size, z))

  def fixST[S, A](k: (=> A) => ST[S, A]): ST[S, A] =
    ST( {
      lazy val ans: A =
        k(r).run

      lazy val r =
        ans

      ans
    } )

  def accumArray[F[_], A: ClassTag, B](size: Int, f: (A, B) => A, z: A, ivs: F[(Int, B)])(implicit F: Foldable[F]): ImmutableArray[A] =
    {
      import std.anyVal.unitInstance

      type STA[S] = ST[S, ImmutableArray[A]]

      runST( new Forall[STA] {
        def apply[S] =
          for {
            a <- newArr(size, z)
            _ <- {
              F.foldMap(ivs)((x: (Int, B)) => a.update(f, x._1, x._2))(stMonoid[S, Unit])
            }
            frozen <- a.freeze
          } yield frozen
      } )
    }
}

sealed abstract class STInstance0  {
  implicit def stSemigroup[S, A](implicit A: Semigroup[A]): Semigroup[ST[S, A]] =
    Semigroup.liftSemigroup[ST[S, *], A](ST.stMonad[S], A)
}

sealed abstract class STInstances  extends STInstance0 {
  implicit def stMonoid[S, A](implicit A: Monoid[A]): Monoid[ST[S, A]] =
    Monoid.liftMonoid[ST[S, *], A](stMonad[S], A)

  implicit def stMonad[S]: Monad[ST[S, *]] =
    new Monad[ST[S, *]] {
      def point[A](a: => A): ST[S, A] =
        returnST(a)

      def bind[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] =
        fa flatMap f
    }
}