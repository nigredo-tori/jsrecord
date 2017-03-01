package jsrecord.operation

import shapeless._
import tag.@@
import labelled.{ FieldType, field }

/**
  * Convert record keys from `Symbol @@ String(..)` to `String(..)`.
  *
  * Useful for APIs relying on [[shapeless.record.RecordArgs]].
  */
object StripArgs extends Poly1 {
  implicit def caseKV[K, V] = at[FieldType[Symbol @@ K, V]](
    f => field[K](f: V)
  )
}
