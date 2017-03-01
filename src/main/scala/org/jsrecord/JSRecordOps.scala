package jsrecord

import scala.scalajs.js

import shapeless._
import record._
import labelled.{ FieldType, field }
import syntax.singleton._

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

  def get(k: Witness)(implicit
    s: ops.record.Selector[M, k.T],
    ev: k.T <:< String
  ): s.Out = {
    self.asInstanceOf[js.Dynamic].selectDynamic(k.value).asInstanceOf[s.Out]
  }

  def selectDynamic(k: Witness)(implicit
    s: ops.record.Selector[M, k.T],
    ev: k.T <:< String
  ): s.Out = this.get(k)

  def toRecord(
    implicit vr: JSRecord.ValidRecord[M]
  ): M = JSRecord.fromJS(self)

  def copy = new CopyOp()
}
