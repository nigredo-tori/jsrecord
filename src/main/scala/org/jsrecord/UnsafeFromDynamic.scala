package jsrecord

import scala.annotation.implicitNotFound
import scala.scalajs.js

import shapeless._
import labelled.{ field, FieldType }

/**
  * A twin of [[shapeless.ops.maps.FromMap]] that provides a conversion from
  * [[js.Dynamic]] to a record. The conversion is unsafe in that it doesn't check
  * neither the source's type, nor field value types. Only use this for objects you
  * control!
  */

@implicitNotFound("Implicit not found: UnsafeFromDynamic[${M}].")
sealed trait UnsafeFromDynamic[M <: HList] {
  def apply(d: js.Dynamic): M
}

object UnsafeFromDynamic {
  def apply[M <: HList](implicit res: UnsafeFromDynamic[M]) = res

  implicit def hnilUFD: UnsafeFromDynamic[HNil] = new UnsafeFromDynamic[HNil] {
    def apply(d: js.Dynamic) = HNil
  }

  implicit def hconsUFD[K <: String, V, T <: HList](
    implicit key: Witness.Aux[K], doTail: UnsafeFromDynamic[T]
  ): UnsafeFromDynamic[FieldType[K, V] :: T] =
    new UnsafeFromDynamic[FieldType[K, V] :: T] {
      def apply(d: js.Dynamic): FieldType[K, V] :: T =
        field[K](d.selectDynamic(key.value).asInstanceOf[V]) :: doTail(d)
    }
}
