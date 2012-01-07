package org.positronicnet.orm

import org.positronicnet.content._
import org.positronicnet.notifications._
import org.positronicnet.util._
import org.positronicnet.facility._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

abstract class BaseJoin[ TLeft <: ManagedRecord, 
                         TRight <: ManagedRecord,
                         TResult ](
    mgrLeft:  BaseRecordManager[TLeft],
    mgrRight: BaseRecordManager[TRight],
    cols:     Seq[String],
    query:    ContentQuery[_,_] 
  )
  extends BaseNotifier( query.facility )
  with NonSharedNotifier[ TResult ]
{
  abstract class ColumnId
  case class LeftCol( s: String )  extends ColumnId
  case class RightCol( s: String ) extends ColumnId

  protected val leftRemappings:  HashMap[String, String] = HashMap.empty
  protected val rightRemappings: HashMap[String, String] = HashMap.empty

  def remap( col: ColumnId, joinCol: String ) =
    col match {
      case LeftCol(s)  => { leftRemappings(s)  = joinCol }
      case RightCol(s) => { rightRemappings(s) = joinCol }
    }

  private def colIdx( col: String, mappings: HashMap[String,String] ) = {
    val renamedCol = mappings.getOrElse( col, col )
    val idx = cols.indexOf( renamedCol )
    if (idx < 0) 
      throw new RuntimeException("Column "+renamedCol+" not present in join")
    idx
  }

  private def remapFields( fields: Seq[MappedField], 
                           remappings: HashMap[String,String]) =
    for (field <- fields)
    yield field.atIndex( colIdx( field.dbColumnName, remappings ))

  protected lazy val leftFields  = remapFields(mgrLeft.fields,  leftRemappings )
  protected lazy val rightFields = remapFields(mgrRight.fields, rightRemappings)

  protected lazy val leftPkIndex  = 
    colIdx( mgrLeft.primaryKeyField.dbColumnName,  leftRemappings  )
  protected lazy val rightPkIndex = 
    colIdx( mgrRight.primaryKeyField.dbColumnName, rightRemappings )
}

class OneToManyJoin[ TLeft <: ManagedRecord,
                     TRight <: ManagedRecord ](
    mgrLeft:  BaseRecordManager[TLeft],
    mgrRight: BaseRecordManager[TRight],
    cols:     Seq[String],
    query:    ContentQuery[_,_]
  )
  extends BaseJoin[ TLeft, TRight, IndexedSeq[ (TLeft, IndexedSeq[TRight]) ]]( 
    mgrLeft, mgrRight, cols, query )
{
  def currentValue: IndexedSeq[(TLeft, IndexedSeq[TRight])] = {

    val leftRecs: ArrayBuffer[ TLeft ] = ArrayBuffer.empty
    val rightRecs: HashMap[ Long, ArrayBuffer[ TRight ]] = HashMap.empty

    for( c <- query.select( cols: _* )) {

      // We expect to have a left record.  We may have seen it already...

      val leftPk = c.getLong( leftPkIndex )

      if (!rightRecs.contains( leftPk )) {
        // Haven't seen it before.
        leftRecs  += mgrLeft.instantiateFrom( c, leftFields )
        rightRecs += ( leftPk -> ArrayBuffer.empty )
      }

      // We may or may not have a right record.  (Always for inner joins,
      // but we may be doing an outer join.)  If we do, deal with it...

      if (!c.isNull( rightPkIndex )) {
        rightRecs( leftPk ) += mgrRight.instantiateFrom( c, rightFields )
      }
    }

    for (rec <- leftRecs)
    yield (rec, rightRecs( rec.id.id ))
  }
}

