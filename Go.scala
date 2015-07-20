import scalaz.Contravariant

trait Go[I,O] { def go(i:I): O }

object Go {

  implicit def GoContravariant[O]: Contravariant[({type l[A]=Go[A,O]})#l] =
    new Contravariant[({type l[A]=Go[A,O]})#l] {
      def contramap[A, B](r: Go[A, O])(f: B => A): Go[B, O] =
        new Go[B, O] { def go(b: B): O = r go f(b) }
    }

  import scalaz.syntax.contravariant._

  val go1 = new Go[String, Int] { def go(a: String) = a.length }
  val go2: Go[List[Char], Int] = go1.contramap { cs: List[Char] => cs.mkString }
}

