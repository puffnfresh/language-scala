package scalaz {
  import scala.annotation.{nowarn}

  trait Foldable[F[_]]  { self =>
    def foldMap[A, B](fa : F[A])(f : (A) => B)(implicit F : Monoid[B]): B
    def foldMap1Opt[A, B](fa : F[A])(f : (A) => B)(implicit F : Semigroup[B]) : Option[B] =
      {
        import std.option.{_}
      
        foldMap(fa)((x) => some(f(x)))
      }
    def foldRight[A, B](fa : F[A], z : => B)(f : (A, => B) => B): B
    def compose[G[_]](implicit G0 : Foldable[G]) : Foldable[λ[(α) => F[G[α]]]] =
      new CompositionFoldable[F, G] {
        implicit def F =
          self
        implicit def G =
          G0
      }
    def bicompose[G[_, _]: Bifoldable] : Bifoldable[λ[(α, β) => F[G[α, β]]]] =
      new CompositionFoldableBifoldable[F, G] {
        def F =
          self
        def G =
          implicitly
      }
    def product[G[_]](implicit G0 : Foldable[G]) : Foldable[λ[(α) => (F[α], G[α])]] =
      new ProductFoldable[F, G] {
        implicit def F =
          self
        implicit def G =
          G0
      }
    def product0[G[_]](implicit G0 : Foldable1[G]) : Foldable1[λ[(α) => (F[α], G[α])]] =
      new ProductFoldable1R[F, G] {
        def F =
          self
        def G =
          G0
      }
    def foldLeft[A, B](fa : F[A], z : B)(f : (B, A) => B) : B =
      {
        import Dual.{_}, Endo.{_}, syntax.std.all.{_}
      
        Tag.unwrap(foldMap(fa)((a : A) => Dual(Endo.endo(f.flip.curried(a))))(dualMonoid)) apply z
      }
    def foldRightM[G[_], A, B](fa : F[A], z : => B)(f : (A, => B) => G[B])(implicit M : Monad[G]) : G[B] =
      foldLeft[A, (B) => G[B]](fa, M.point(_))((b, a) => (w) => M.bind(f(a, w))(b))(z)
    def foldLeftM[G[_], A, B](fa : F[A], z : B)(f : (B, A) => G[B])(implicit M : Monad[G]) : G[B] =
      foldRight[A, (B) => G[B]](fa, M.point(_))((a, b) => (w) => M.bind(f(w, a))(b))(z)
    def foldMapM[G[_], A, B](fa : F[A])(f : (A) => G[B])(implicit B : Monoid[B], G : Monad[G]) : G[B] =
      foldRightM[G, A, B](fa, B.zero)((a, b2) => G.map(f(a))((b1) => B.append(b1, b2)))
    def fold[M: Monoid](t : F[M]) : M =
      foldMap[M, M](t)((x) => x)
    def fold1Opt[A: Semigroup](fa : F[A]) : Option[A] =
      foldMap1Opt(fa)((a) => a)
    def traverse_[M[_], A, B](fa : F[A])(f : (A) => M[B])(implicit a : Applicative[M]) : M[Unit] =
      foldLeft(fa, a.pure(()))((x, y) => a.ap(f(y))(a.map(x)((_) => (_) => ())))
    final def traverseU_[A, GB](fa : F[A])(f : (A) => GB)(implicit G : Unapply[Applicative, GB]) : G.M[Unit] =
      traverse_[G.M, A, G.A](fa)(G.leibniz.onF(f))(G.TC)
    def traverseS_[S, A, B](fa : F[A])(f : (A) => State[S, B]) : State[S, Unit] =
      State( {
        (s) => (foldLeft(fa, s)((s, a) => f(a)(s)._1), ())
      } )
    def sequence_[M[_], A](fa : F[M[A]])(implicit a : Applicative[M]) : M[Unit] =
      traverse_(fa)((x) => x)
    def sequenceS_[S, A](fga : F[State[S, A]]) : State[S, Unit] =
      traverseS_(fga)((x) => x)
    def sequenceF_[M[_], A](ffa : F[Free[M, A]]) : Free[M, Unit] =
      foldLeft[Free[M, A], Free[M, Unit]](ffa, Free.pure[M, Unit](()))((c, d) => c.flatMap((_) => d.map((_) => ())))
    final def foldr[A, B](fa : F[A], z : => B)(f : (A) => (=> B) => B) : B =
      foldRight(fa, z)((a, b) => f(a)(b))
    def foldMapRight1Opt[A, B](fa : F[A])(z : (A) => B)(f : (A, => B) => B) : Option[B] =
      foldRight(fa, None : Option[B])((a, optB) => optB map f(a, _) orElse Some(z(a)))
    def foldRight1Opt[A](fa : F[A])(f : (A, => A) => A) : Option[A] =
      foldMapRight1Opt(fa)(identity)(f)
    def foldr1Opt[A](fa : F[A])(f : (A) => (=> A) => A) : Option[A] =
      foldRight(fa, None : Option[A])((a, optA) => optA map ((aa) => f(a)(aa)) orElse Some(a))
    final def foldl[A, B](fa : F[A], z : B)(f : (B) => (A) => B) : B =
      foldLeft(fa, z)((b, a) => f(b)(a))
    def foldMapLeft1Opt[A, B](fa : F[A])(z : (A) => B)(f : (B, A) => B) : Option[B] =
      foldLeft(fa, None : Option[B])((optB, a) => optB map f(_, a) orElse Some(z(a)))
    def foldLeft1Opt[A](fa : F[A])(f : (A, A) => A) : Option[A] =
      foldMapLeft1Opt(fa)(identity)(f)
    def foldl1Opt[A](fa : F[A])(f : (A) => (A) => A) : Option[A] =
      foldLeft(fa, None : Option[A])((optA, a) => optA map ((aa) => f(aa)(a)) orElse Some(a))
    final def foldrM[G[_], A, B](fa : F[A], z : => B)(f : (A) => (=> B) => G[B])(implicit M : Monad[G]) : G[B] =
      foldRightM(fa, z)((a, b) => f(a)(b))
    final def foldlM[G[_], A, B](fa : F[A], z : => B)(f : (B) => (A) => G[B])(implicit M : Monad[G]) : G[B] =
      foldLeftM(fa, z)((b, a) => f(b)(a))
    final def findMapM[M[_]: Monad, A, B](fa : F[A])(f : (A) => M[Option[B]]) : M[Option[B]] =
      toEphemeralStream(fa) findMapM f
    def findLeft[A](fa : F[A])(f : (A) => Boolean) : Option[A] =
      foldLeft[A, Option[A]](fa, None)( (b, a) => b.orElse( if (f(a))
        Some(a)
      else
        None ) )
    def findRight[A](fa : F[A])(f : (A) => Boolean) : Option[A] =
      foldRight[A, Option[A]](fa, None)( (a, b) => b.orElse( if (f(a))
        Some(a)
      else
        None ) )
    final def count[A](fa : F[A]) : Int =
      length(fa)
    def length[A](fa : F[A]) : Int =
      foldLeft(fa, 0)((b, _) => b + 1)
    def index[A](fa : F[A], i : Int) : Option[A] =
      foldLeft[A, (Int, Option[A])](fa, (0, None))( {
        case ((idx, elem), curr) =>
          ( idx + 1
          , elem orElse {
            if (idx == i)
              Some(curr)
            else
              None
          } )
      } )._2
    def indexOr[A](fa : F[A], default : => A, i : Int) : A =
      index(fa, i) getOrElse default
    def toList[A](fa : F[A]) : List[A] =
      {
        foldLeft(fa, List.newBuilder[A])(_ += (_)).result()
      }
    def toVector[A](fa : F[A]) : Vector[A] =
      {
        foldLeft(fa, Vector.newBuilder[A])(_ += (_)).result()
      }
    def toSet[A](fa : F[A]) : Set[A] =
      {
        foldLeft(fa, Set.newBuilder[A])(_ += (_)).result()
      }
    @nowarn("since=2.13.0") def toStream[A](fa : F[A]) : Stream[A] =
      foldRight[A, Stream[A]](fa, Stream.empty)(Stream.cons(_, _))
    def toLazyList[A](fa : F[A]) : LazyList[A] =
      foldRight[A, LazyList[A]](fa, LazyList.empty)(LazyList.cons(_, _))
    def toIList[A](fa : F[A]) : IList[A] =
      foldLeft(fa, IList.empty[A])((t, h) => h :: t).reverse
    def toEphemeralStream[A](fa : F[A]) : EphemeralStream[A] =
      foldRight(fa, EphemeralStream.emptyEphemeralStream[A])(EphemeralStream.cons(_, _))
    def all[A](fa : F[A])(p : (A) => Boolean) : Boolean =
      foldRight(fa, true)(p(_) && (_))
    def allM[G[_], A](fa : F[A])(p : (A) => G[Boolean])(implicit G : Monad[G]) : G[Boolean] =
      foldRight(fa, G.point(true))( (a, b) => G.bind(p(a))( (q) => if (q)
        b
      else
        G.point(false) ) )
    def any[A](fa : F[A])(p : (A) => Boolean) : Boolean =
      foldRight(fa, false)(p(_) || (_))
    def anyM[G[_], A](fa : F[A])(p : (A) => G[Boolean])(implicit G : Monad[G]) : G[Boolean] =
      foldRight(fa, G.point(false))( (a, b) => G.bind(p(a))( (q) => if (q)
        G.point(true)
      else
        b ) )
    def filterLength[A](fa : F[A])(f : (A) => Boolean) : Int =
      foldLeft(fa, 0)( (b, a) => (if (f(a))
        1
      else
        0) + b )
    import Ordering.{GT, LT}
    import std.option.{some, none}
    def maximum[A: Order](fa : F[A]) : Option[A] =
      foldLeft(fa, none[A])( {
        case (None, y) =>
          some(y)
        case (Some (x), y) =>
          some( if (Order[A].order(x, y) == GT)
            x
          else
            y )
      } )
    def maximumOf[A, B: Order](fa : F[A])(f : (A) => B) : Option[B] =
      foldLeft(fa, none[B])( {
        case (None, a) =>
          some(f(a))
        case (Some (b), aa) =>
          {
            val bb =
              f(aa)
          
            some( if (Order[B].order(b, bb) == GT)
              b
            else
              bb )
          }
      } )
    def maximumBy[A, B: Order](fa : F[A])(f : (A) => B) : Option[A] =
      foldLeft(fa, none[(A, B)])( {
        case (None, a) =>
          some(a -> f(a))
        case (Some (x @ (a, b)), aa) =>
          {
            val bb =
              f(aa)
          
            some( if (Order[B].order(b, bb) == GT)
              x
            else
              aa -> bb )
          }
      } ) map (_._1)
    def minimum[A: Order](fa : F[A]) : Option[A] =
      foldLeft(fa, none[A])( {
        case (None, y) =>
          some(y)
        case (Some (x), y) =>
          some( if (Order[A].order(x, y) == LT)
            x
          else
            y )
      } )
    def minimumOf[A, B: Order](fa : F[A])(f : (A) => B) : Option[B] =
      foldLeft(fa, none[B])( {
        case (None, a) =>
          some(f(a))
        case (Some (b), aa) =>
          {
            val bb =
              f(aa)
          
            some( if (Order[B].order(b, bb) == LT)
              b
            else
              bb )
          }
      } )
    def minimumBy[A, B: Order](fa : F[A])(f : (A) => B) : Option[A] =
      foldLeft(fa, none[(A, B)])( {
        case (None, a) =>
          some(a -> f(a))
        case (Some (x @ (a, b)), aa) =>
          {
            val bb =
              f(aa)
          
            some( if (Order[B].order(b, bb) == LT)
              x
            else
              aa -> bb )
          }
      } ) map (_._1)
    def extrema[A: Order](fa : F[A]) : Option[(A, A)] =
      extremaBy(fa)(identity)
    def extremaOf[A, B: Order](fa : F[A])(f : (A) => B) : Option[(B, B)] =
      foldMapLeft1Opt(fa)( {
        (a) => {
          val b =
            f(a)
        
          (b, b)
        }
      } )( {
        case (x @ (bmin, bmax), a) =>
          {
            val b =
              f(a)
          
            if (Order[B].order(b, bmin) == LT)
              (b, bmax)
            else
              if (Order[B].order(b, bmax) == GT)
                (bmin, b)
              else
                x
          }
      } )
    def extremaBy[A, B: Order](fa : F[A])(f : (A) => B) : Option[(A, A)] =
      foldMapLeft1Opt(fa)( {
        (a) => {
          val b =
            f(a)
        
          (a, a, b, b)
        }
      } )( {
        case (x @ (amin, amax, bmin, bmax), a) =>
          {
            val b =
              f(a)
          
            val greaterThanOrEq =
              Order[B].greaterThanOrEqual(b, bmax)
          
            if (Order[B].lessThanOrEqual(b, bmin))
              {
                if (greaterThanOrEq)
                  {
                    (a, a, b, b)
                  }
                else
                  {
                    (a, amax, b, bmax)
                  }
              }
            else
              {
                if (greaterThanOrEq)
                  {
                    (amin, a, bmin, b)
                  }
                else
                  {
                    x
                  }
              }
          }
      } ) map {
        case (amin, amax, _, _) =>
          (amin, amax)
      }
    def sumr[A](fa : F[A])(implicit A : Monoid[A]) : A =
      foldRight(fa, A.zero)(A.append)
    def sumr1Opt[A](fa : F[A])(implicit A : Semigroup[A]) : Option[A] =
      foldRight1Opt(fa)(A.append(_, _))
    def suml[A](fa : F[A])(implicit A : Monoid[A]) : A =
      foldLeft(fa, A.zero)(A.append(_, _))
    def suml1Opt[A](fa : F[A])(implicit A : Semigroup[A]) : Option[A] =
      foldLeft1Opt(fa)(A.append(_, _))
    def psumMap[A, B, G[_]](fa : F[A])(f : (A) => G[B])(implicit G : PlusEmpty[G]) : G[B] =
      foldMap(fa)(f)(G.monoid)
    def psum[G[_], A](fa : F[G[A]])(implicit G : PlusEmpty[G]) : G[A] =
      fold(fa)(G.monoid)
    final def asum[G[_], A](fa : F[G[A]])(implicit G : PlusEmpty[G]) : G[A] =
      psum(fa)
    def longDigits[A](fa : F[A])(implicit d : A <:< Digit) : Long =
      foldLeft(fa, 0L)((n, a) => n * 10L + (a : Digit))
    def empty[A](fa : F[A]) : Boolean =
      all(fa)((_) => false)
    def element[A: Equal](fa : F[A], a : A) : Boolean =
      any(fa)(Equal[A].equal(a, _))
    def intercalate[A](fa : F[A], a : A)(implicit A : Monoid[A]) : A =
      foldRight(fa, none[A])( {
        (l, oa) => some(A.append(l, oa map A.append(a, _) getOrElse A.zero))
      } ).getOrElse(A.zero)
    def splitWith[A](fa : F[A])(p : (A) => Boolean) : List[NonEmptyList[A]] =
      foldRight(fa, Maybe.empty[(NonEmptyList[NonEmptyList[A]], Boolean)])( (a, b) => {
        val pa =
          p(a)
      
        Maybe.just( ( b match {
          case Maybe.Just ((x, q)) =>
            if (pa == q)
              NonEmptyList.nel(a <:: x.head, x.tail)
            else
              NonEmptyList(a) <:: x
          case Maybe.Empty () =>
            NonEmptyList(NonEmptyList(a))
        }
        , pa ) )
      } ).cata(_._1.list.toList, List.empty)
    def splitBy[A, B: Equal](fa : F[A])(f : (A) => B) : IList[(B, NonEmptyList[A])] =
      foldRight(fa, IList[(B, NonEmptyList[A])]())( (a, bas) => {
        val fa =
          f(a)
      
        bas match {
          case INil () =>
            IList.single((fa, NonEmptyList.nel(a, IList.empty)))
          case ICons ((b, as), tail) =>
            if (Equal[B].equal(fa, b))
              ICons((b, a <:: as), tail)
            else
              ICons((fa, NonEmptyList.nel(a, IList.empty)), bas)
        }
      } )
    def splitByRelation[A](fa : F[A])(r : (A, A) => Boolean) : IList[NonEmptyList[A]] =
      foldRight(fa, IList[NonEmptyList[A]]())( (a, neas) => neas match {
        case INil () =>
          IList.single(NonEmptyList.nel(a, IList.empty))
        case ICons (nea, tail) =>
          if (r(a, nea.head))
            ICons(a <:: nea, tail)
          else
            ICons(NonEmptyList.nel(a, IList.empty), neas)
      } )
    def selectSplit[A](fa : F[A])(p : (A) => Boolean) : List[NonEmptyList[A]] =
      {
        import scalaz.syntax.foldable.{_}
      
        def squash(t : (List[NonEmptyList[A]], IList[A])) : List[NonEmptyList[A]] =
          t._2.toNel.toList ::: t._1
      
        squash( foldRight(fa, (List.empty[NonEmptyList[A]], IList.empty[A]))( (a, l) => if (p(a))
          (l._1, a :: l._2)
        else
          (squash(l), IList.empty) ) )
      }
    def distinct[A](fa : F[A])(implicit A : Order[A]) : IList[A] =
      foldLeft(fa, (ISet.empty[A], IList.empty[A]))( {
        case ((seen, acc), a) =>
          if (seen.notMember(a))
            (seen.insert(a), a :: acc)
          else
            (seen, acc)
      } )._2.reverse
    def distinctE[A](fa : F[A])(implicit A : Equal[A]) : IList[A] =
      foldLeft(fa, IList.empty[A])( {
        case (seen, a) =>
          if (!IList.instances.element(seen, a))
            a :: seen
          else
            seen
      } ).reverse
    def distinctBy[A, B: Equal](fa : F[A])(f : (A) => B) : IList[A] =
      distinctE(fa)(Equal.equalBy(f))
    def collapse[X[_], A](x : F[A])(implicit A : ApplicativePlus[X]) : X[A] =
      foldRight(x, A.empty[A])((a, b) => A.plus(A.point(a), b))
    trait FoldableLaw  {
      import std.vector.{_}
      def leftFMConsistent[A: Equal](fa : F[A]) : Boolean =
        Equal[Vector[A]].equal(foldMap(fa)(Vector(_)), foldLeft(fa, Vector.empty[A])(_ :+ (_)))
      def rightFMConsistent[A: Equal](fa : F[A]) : Boolean =
        Equal[Vector[A]].equal(foldMap(fa)(Vector(_)), foldRight(fa, Vector.empty[A])(_ +: (_)))
    }
    def foldableLaw : FoldableLaw =
      new FoldableLaw {
      }
    val foldableSyntax : scalaz.syntax.FoldableSyntax[F] =
      new scalaz.syntax.FoldableSyntax[F] {
        def F =
          Foldable.this
      }
  }

  object Foldable {
    @inline def apply[F[_]](implicit F : Foldable[F]) : Foldable[F] =
      F
    def fromIso[F[_], G[_]](D : F ~> G)(implicit E : Foldable[G]) : Foldable[F] =
      new IsomorphismFoldable[F, G] {
        override def G : Foldable[G] =
          E
        override def naturalTrans : F ~> G =
          D
      }
    trait FromFoldMap[F[_]]  extends Foldable[F] {
      override def foldRight[A, B](fa : F[A], z : => B)(f : (A, => B) => B) =
        foldMap(fa)((a : A) => Endo.endoByName[B](f(a, _))) apply z
    }
    trait FromFoldr[F[_]]  extends Foldable[F] {
      override def foldMap[A, B](fa : F[A])(f : (A) => B)(implicit F : Monoid[B]) =
        foldRight[A, B](fa, F.zero)((x, y) => F.append(f(x), y))
    }
  }

  trait IsomorphismFoldable[F[_], G[_]]  extends Foldable[F] {
    implicit def G: Foldable[G]
    protected[this] def naturalTrans: F ~> G
    override def foldMap[A, B](fa : F[A])(f : (A) => B)(implicit F : Monoid[B]) : B =
      G.foldMap(naturalTrans(fa))(f)
    override def foldLeft[A, B](fa : F[A], z : B)(f : (B, A) => B) : B =
      G.foldLeft(naturalTrans(fa), z)(f)
    override def foldRight[A, B](fa : F[A], z : => B)(f : (A, => B) => B) : B =
      G.foldRight[A, B](naturalTrans(fa), z)(f)
  }
}