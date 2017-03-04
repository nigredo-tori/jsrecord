package jsrecord

import scalaz.Liskov
import Liskov.<~<

import shapeless._

trait OrphanInstances {

  implicit def hconsLiskov[LH, LT <: HList, RH, RT <: HList](
    implicit
      h: LH <~< RH,
    t: LT <~< RT
  ): (LH :: LT) <~< (RH :: RT) =
    // We can't provide a good instance because of type bounds
    Liskov.force
}
