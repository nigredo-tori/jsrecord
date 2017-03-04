package jsrecord

import scala.scalajs.js

import shapeless._
import record._
import labelled.{ FieldType, field }
import syntax.singleton._

import scalaz.Liskov
import Liskov.{ witness, `<~<` }

import jsrecord.operation.{ StripArgs }

class JSRecordOps[M <: HList](self: JSRecord[M]) {

  class CopyOp extends RecordArgs {
    def applyRecord[R0 <: HList, R <: HList](r0: R0)(
      implicit
        mapper: ops.hlist.Mapper.Aux[StripArgs.type, R0, R],
      merge: ops.record.Merger.Aux[M, R, M],
      vr: JSRecord.ValidRecord[M]
    ): JSRecord[M] =
      JSRecord.toJS(merge(self.toRecord, mapper(r0)))
  }

  // Three ways to access a field:
  // r("foo")
  // r.get("foo")
  // r.foo

  def get[K](k: Witness.Aux[K])(implicit
    s: ops.record.Selector[M, K],
    ev: K <:< String
  ): s.Out =
    JSRecord.get(k, self)

  def apply[K](k: Witness.Aux[K])(implicit
    s: ops.record.Selector[M, K],
    ev: K <:< String
  ): s.Out =
    JSRecord.get(k, self)

  def selectDynamic[K](k: Witness.Aux[K])(implicit
    s: ops.record.Selector[M, K],
    ev: K <:< String
  ): s.Out =
    JSRecord.get(k, self)

  def toRecord(
    implicit vr: JSRecord.ValidRecord[M]
  ): M = JSRecord.fromJS(self)

  def copy = new CopyOp()

  // R is generally another JSRecord - but it's way easier to just make this
  // a syntax for Liskov
  def as[R](implicit ev: JSRecord[M] <~< R): R =
    // Implicit conversion provided by Liskov
    Liskov.witness(ev)(self)
}
