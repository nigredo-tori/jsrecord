package jsrecord.operation

import shapeless._

/**
  * Replaces all element types in [[HList]] type with a provided types.
  *
  * This is stand-in for `Mapped[?, Const[?]#λ]`, which seems to be broken.
  */
trait ReplaceAll[L <: HList, X] {
  type Out <: HList
}

object ReplaceAll {

  type Aux[L <: HList, X, R <: HList] = ReplaceAll[L, X] { type Out = R }

  implicit def replaceHNil[X]: Aux[HNil, X, HNil] = new ReplaceAll[HNil, X] {
    type Out = HNil
  }

  implicit def replaceHCons[H, T <: HList, X, TR <: HList](
    implicit t: Aux[T, X, TR]
  ): Aux[H :: T, X, X :: TR] = new ReplaceAll[H :: T, X] {
    type Out = X :: TR
  }
}
