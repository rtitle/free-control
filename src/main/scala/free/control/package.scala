package free

import cats.data.Coproduct
import cats.free.{Free, Inject}

/**
  * Created by rtitle on 7/15/17.
  */
package object control {

  implicit def controlFlow[F[_]](implicit I: Inject[ControlFlowA, F]): ControlFlow[F] = new ControlFlow[F]

  type ControlFlowApp[F[_], A] = Coproduct[F, ControlFlowA, A]
  type FreeControlFlowApp[F[_], A] = Free[ControlFlowApp[F, ?], A]

}
