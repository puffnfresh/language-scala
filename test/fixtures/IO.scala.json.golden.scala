package scalaz {
  package effect {
    import IvoryTower.{_}
  
    import RegionT.{_}
  
    import RefCountedFinalizer.{_}
  
    import FinalizerHandle.{_}
  
    import ST.{_}
  
    import Kleisli.{_}
  
    import Free.{_}
  
    import std.function.{_}
  
    sealed abstract class IO[A]  {
      private[effect] def apply(rw : Tower[IvoryTower]): Trampoline[(Tower[IvoryTower], A)]
      import IO.{_}
      def unsafePerformIO() : A =
        apply(ivoryTower).run._2
      def unsafeInterleaveIO() : IO[Trampoline[A]] =
        IO(apply(ivoryTower).map(_._2))
      def unsafeZipWith[B, C](iob : IO[B], f : (A, B) => C) : IO[C] =
        for {
        
          a <- unsafeInterleaveIO()
        
          b <- iob.unsafeInterleaveIO()
        
          c <- io((rw) => a.zipWith(b)((x, y) => rw -> f(x, y)))
        
        } yield c
      def unsafeZip[B](iob : IO[B]) : IO[(A, B)] =
        unsafeZipWith(iob, Tuple2[A, B])
      def unsafeZip_[B](iob : IO[B]) : IO[B] =
        unsafeZipWith(iob, (a : A, b : B) => b)
      def map[B](f : (A) => B) : IO[B] =
        io( (rw) => apply(rw) map {
          case (nw, a) =>
            (nw, f(a))
        } )
      def flatMap[B](f : (A) => IO[B]) : IO[B] =
        io( (rw) => apply(rw) flatMap {
          case (nw, a) =>
            f(a)(nw)
        } )
      def liftIO[M[_]](implicit m : LiftIO[M]) : M[A] =
        m.liftIO(this)
      def except(handler : (Throwable) => IO[A]) : IO[A] =
        io( (rw) => try {
          Free.pure(this(rw).run)
        } catch {
          case e: Throwable =>
            handler(e)(rw)
        } )
      def catchSome[B](p : (Throwable) => Option[B], handler : (B) => IO[A]) : IO[A] =
        except( (e) => p(e) match {
          case Some (z) =>
            handler(z)
          case None =>
            throw e
        } )
      def catchLeft : IO[Throwable \/ A] =
        map(\/.right[Throwable, A]) except ((t) => IO(-\/(t)))
      def catchSomeLeft[B](p : (Throwable) => Option[B]) : IO[B \/ A] =
        catchLeft map (_.leftMap((e) => p(e).getOrElse(throw e)))
      def onException[B](action : IO[B]) : IO[A] =
        this except ((e) => for {
        
          _ <- action
        
          a <- (throw e) : IO[A]
        
        } yield a)
      def bracket[B, C](after : (A) => IO[B])(during : (A) => IO[C]) : IO[C] =
        for {
        
          a <- this
        
          r <- during(a) onException after(a)
        
          _ <- after(a)
        
        } yield r
      def ensuring[B](sequel : IO[B]) : IO[A] =
        for {
        
          r <- onException(sequel)
        
          _ <- sequel
        
        } yield r
      def bracket_[B, C](after : IO[B])(during : IO[C]) : IO[C] =
        bracket((_) => after)((_) => during)
      def bracketOnError[B, C](after : (A) => IO[B])(during : (A) => IO[C]) : IO[C] =
        for {
        
          a <- this
        
          r <- during(a) onException after(a)
        
        } yield r
      def bracketIO[M[_], B](after : (A) => IO[Unit])(during : (A) => M[B])(implicit m : MonadControlIO[M]) : M[B] =
        controlIO((runInIO : RunInBase[M, IO]) => bracket(after)(runInIO.apply compose during))
      def using[C](f : (A) => IO[C])(implicit resource : Resource[A]) : IO[C] =
        bracket(resource.close)(f)
    }
  
    sealed abstract class IOInstances1  {
      implicit def IOSemigroup[A](implicit A : Semigroup[A]) : Semigroup[IO[A]] =
        Semigroup.liftSemigroup[IO, A](IO.ioMonad, A)
      implicit val iOLiftIO : LiftIO[IO] =
        new IOLiftIO {
        }
      implicit val ioMonad : Monad[IO] with BindRec[IO] =
        new IOMonad {
        }
    }
  
    sealed abstract class IOInstances0  extends IOInstances1 {
      implicit def IOMonoid[A](implicit A : Monoid[A]) : Monoid[IO[A]] =
        Monoid.liftMonoid[IO, A](ioMonad, A)
      implicit val ioMonadIO : MonadIO[IO] =
        new MonadIO[IO] with IOLiftIO with IOMonad {
        }
    }
  
    sealed abstract class IOInstances  extends IOInstances0 {
      implicit val ioMonadCatchIO : MonadCatchIO[IO] =
        new IOMonadCatchIO with IOLiftIO with IOMonad {
        }
    }
  
    private trait IOMonad  extends Monad[IO] with BindRec[IO] {
      def point[A](a : => A) : IO[A] =
        IO(a)
      override def map[A, B](fa : IO[A])(f : (A) => B) =
        fa map f
      def bind[A, B](fa : IO[A])(f : (A) => IO[B]) : IO[B] =
        fa flatMap f
      def tailrecM[A, B](a : A)(f : (A) => IO[A \/ B]) : IO[B] =
        IO.tailrecM(a)(f)
    }
  
    private trait IOLiftIO  extends LiftIO[IO] {
      def liftIO[A](ioa : IO[A]) =
        ioa
    }
  
    private trait IOMonadCatchIO  extends MonadCatchIO[IO] {
      def except[A](io : IO[A])(h : (Throwable) => IO[A]) : IO[A] =
        io.except(h)
    }
  
    object IO extends IOInstances {
      def apply[A](a : => A) : IO[A] =
        io((rw) => return_(rw -> a))
      def getChar : IO[Char] =
        IO( {
          val s =
            scala.Console.in.readLine()
        
          if (s == null)
            throw new java.io.EOFException("Console has reached end of input")
          else
            s charAt 0
        } )
      def putChar(c : Char) : IO[Unit] =
        io((rw) => return_(rw -> print(c)))
      def putStr(s : String) : IO[Unit] =
        io((rw) => return_(rw -> print(s)))
      def putStrLn(s : String) : IO[Unit] =
        io((rw) => return_(rw -> println(s)))
      def readLn : IO[String] =
        IO(scala.Console.in.readLine())
      def put[A](a : A)(implicit S : Show[A]) : IO[Unit] =
        io((rw) => return_(rw -> print(S shows a)))
      def putLn[A](a : A)(implicit S : Show[A]) : IO[Unit] =
        io((rw) => return_(rw -> println(S shows a)))
      type RunInBase[M[_], Base[_]] = Forall[λ[(α) => (M[α]) => Base[M[α]]]]
      import scalaz.Isomorphism.{<~>}
      def hoistRunInBase[F[_], G[_]](iso : F <~> G)(r : RunInBase[G, IO]) : RunInBase[F, IO] =
        new RunInBase[F, IO] {
          def apply[B] =
            (x : F[B]) => r.apply(iso.to(x)).map(iso.from(_))
        }
      def io[A](f : (Tower[IvoryTower]) => Trampoline[(Tower[IvoryTower], A)]) : IO[A] =
        new IO[A] {
          private[effect] def apply(rw : Tower[IvoryTower]) =
            Free(() => f(rw))
        }
      def newIORef[A](a : => A) : IO[IORef[A]] =
        STToIO(newVar(a)) flatMap ((v) => IO(IORef.ioRef(v)))
      def throwIO[A](e : Throwable) : IO[A] =
        IO(throw e)
      def idLiftControl[M[_], A](f : (RunInBase[M, M]) => M[A])(implicit m : Monad[M]) : M[A] =
        f(new RunInBase[M, M] {   def apply[B] =   (x : M[B]) => m.point(x) })
      def controlIO[M[_], A](f : (RunInBase[M, IO]) => IO[M[A]])(implicit M : MonadControlIO[M]) : M[A] =
        M.join(M.liftControlIO(f))
      def onExit[S, P[_]: MonadIO](finalizer : IO[Unit]) : RegionT[S, P, FinalizerHandle[RegionT[S, P, *]]] =
        regionT( kleisli( (hsIORef) => (for {
        
          refCntIORef <- newIORef(1)
        
          h = refCountedFinalizer(finalizer, refCntIORef)
        
          _ <- hsIORef.mod(h :: (_))
        
        } yield finalizerHandle[RegionT[S, P, *]](h)).liftIO[P] ) )
      def runRegionT[P[_]: MonadControlIO, A](r : Forall[RegionT[*, P, A]]) : P[A] =
        {
          def after(hsIORef : IORef[IList[RefCountedFinalizer]]) =
            for {
            
              hs <- hsIORef.read
            
              _ <- hs.foldRight[IO[Unit]](IO.ioUnit)( {
                case (r, o) =>
                  for {
                  
                    refCnt <- r.refcount.mod(_ - 1)
                  
                  } yield ()
              } )
            
            } yield ()
        
          newIORef(IList[RefCountedFinalizer]()).bracketIO(after)((s) => r.apply.value.run(s))
        }
      def tailrecM[A, B](a : A)(f : (A) => IO[A \/ B]) : IO[B] =
        io( (rw) => BindRec[Trampoline].tailrecM[(Tower[IvoryTower], A), (Tower[IvoryTower], B)]((rw, a))( {
          case (nw0, x) =>
            f(x)(nw0).map( {
              case (nw1, e) =>
                e.bimap((nw1, _), (nw1, _))
            } )
        } ) )
      implicit def IOToST[A](io : IO[A]) : ST[IvoryTower, A] =
        st(() => io(ivoryTower).run._2)
      val ioUnit : IO[Unit] =
        IO(())
    }
  }
}