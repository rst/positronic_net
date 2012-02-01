package org.positronicnet.orm

import android.content.{ContentProviderOperation, ContentValues}
import android.net.Uri

import org.positronicnet.orm.Actions._
import org.positronicnet.content.{PositronicContentResolver,
                                  ContentProviderQuery,
                                  BatchAction}

import scala.collection.mutable.HashMap

/** A subclass of [[org.positronicnet.content.BatchAction]] that allows
  * callers to batch up `Save` and `Delete` operations in the ORM.
  * You can still also add raw `ContentProviderOperation`s, including
  * those of other types, most notably "assert".
  *
  * Note that a `Save` operation will translate into either an insert
  * or update, as appropriate.
  *
  * Note also that if you add a `Save` of, say, an unsaved `RawContact`
  * to a batch, and then add `Save`s of `ContactData` objects which
  * refer to the `RawContact`, references to the ID of the `RawContact`
  * will automatically be turned into back-references.  This allows
  * a collection of related objects to be inserted in a single batch,
  * per recommended best practice for dealing with Contacts (and
  * probably other things).
  */

class BatchScopeAction( authority: String ) 
  extends BatchAction( authority )
  with ScopeBatchTranslation
{
  // The magic actually happens in the ScopeBatchTranslation trait below,
  // which works on things that "look like" a ContentProviderOperation.Builder.
  // That can be the real thing, as here, so we set up the glue code.
  // (It can also be something else, almost entirely for testing purposes.)

  private [orm] type Operation = ContentProviderOperation.Builder

  private [orm] def numPriorOps = this.operations.size

  private [orm]
  def newInsert( uri: Uri ) = ContentProviderOperation.newInsert( uri )
  private [orm]
  def newUpdate( uri: Uri ) = ContentProviderOperation.newUpdate( uri )
  private [orm]
  def newDelete( uri: Uri ) = ContentProviderOperation.newDelete( uri )
  
  private [orm]
  def addValues( op: Operation, cv: ContentValues ) = op.withValues( cv )

  private [orm]
  def addValueBackReference( op: Operation, key: String, backref: Int ): Unit =
    op.withValueBackReference( key, backref )

  private [orm]
  def addSelection( op: Operation, sel: String, selArgs: Array[String] ): Unit =
    op.withSelection( sel, selArgs )

  private [orm]
  def addOperation( op: Operation ) = this.add( op.build )
}

// Logic of translating ScopeActions to ContentProviderOperations,
// except abstracted so that it can also translate to other things
// that *look* like ContentProviderOperations for testing purposes,
// and future DB batch support.  (Real ContentProviderOperations are
// a little *too* opaque to be easily testable; doing things this way
// lets us apply the same logic to more transparent mocks.)

private [orm]
trait ScopeBatchTranslation
{
  // What we expect from any class that we get mixed into

  private [orm] type Operation

  private [orm] def numPriorOps: Int

  private [orm] def newInsert( uri: Uri ): Operation
  private [orm] def newUpdate( uri: Uri ): Operation
  private [orm] def newDelete( uri: Uri ): Operation

  private [orm] def addValues( op: Operation, cv: ContentValues )
  private [orm] def addValueBackReference( op: Operation,
                                           key: String, backref: Int )
  private [orm] def addSelection( op: Operation, 
                                  sel: String, selArgs: Array[String] )

  private [orm] def addOperation( op: Operation ): Unit

  // The translation logic.

  private [orm]
  val opBackRefs = new HashMap[ RecordId[_], Int ]

  def add( op: ScopeAction[_] ): Unit =
    op match {

      case Save( rec ) => 

        val record = rec.asInstanceOf[ ManagedRecord ]
        if (record.isNewRecord) opBackRefs( record.id ) = numPriorOps
        this.addOperation( newSaveRecord( record ))

      case Delete( rec ) => 

        this.addOperation( newDeleteRecord( rec.asInstanceOf[ ManagedRecord ] ))
    }

  private [orm]
  def newSaveRecord[ T <: ManagedRecord ]( record: T ) = {

    // We're doing an insert or an update depending on "record.isnewRecord"

    val mgr = record.id.mgr.asInstanceOf[ BaseRecordManager[T] ]
    val pkey = mgr.primaryKeyField
    val uri = mgrContentUri( mgr )
    val op =
      if (record.isNewRecord) 
        this.newInsert( uri )
      else {
        val update = this.newUpdate( uri )
        addRecordSelection( update, record )
        update
      }

    // Get the right set of fields to write, in either case.  (This is
    // where write-once fields get excluded from updates.)

    val fields = 
      if (record.isNewRecord) mgr.fieldsForInsert else mgr.fieldsForUpdate

    // Now, handle the fields, substituting in back-references for ID fields
    // as necessary.  (Would be better to use 'dataPairs', which would require
    // some extra hooks into ContentValue and derived subclasses.)

    val cv = new ContentValues

    for (mappedField <- fields) {
      mappedField match {
        case idField: MappedIdField =>
          val fieldValue = mappedField.recordField.get( record )
          val idValue = fieldValue.asInstanceOf[ RecordId[_] ]
          opBackRefs.get( idValue ) match {
            case Some( backRef ) =>
              addValueBackReference( op, idField.dbColumnName, backRef )
            case None =>
              if (idValue.isNewRecord)
                // Just store null?  Or leave it out?
                throw new RuntimeException( 
                  "Can't store reference to unsaved record" )
              else
                cv.put( idField.dbColumnName, new java.lang.Long( idValue.id ))
          }
        case _ =>
          val value = mappedField.getValue( record )
          value.putContentValues( cv, mappedField.dbColumnName )
      }
    }

    // And extra data that the record manager may want to toss on...
    // currently discriminator values for VariantRecordManagers.
    // (Which is good, because this might be problematic for 
    // non-string discriminators.)

    for ((key, value) <- mgr.extraDataPairs( record ))
      value.putContentValues( cv, key )

    addValues( op, cv )

    // done

    op
  }

  private [orm]
  def newDeleteRecord[ T <: ManagedRecord ]( record: T ) = {

    val mgr = record.id.mgr.asInstanceOf[ BaseRecordManager[T] ]
    val op = this.newDelete( mgrContentUri( mgr ) )

    addRecordSelection( op, record )
    op
  }

  private [orm]
  def mgrContentUri( mgr: BaseRecordManager[_] ) =
    mgr.baseQuery match {
      case crQuery: ContentProviderQuery[_] => crQuery.contentUri
      case _ => throw new RuntimeException( 
        "can only do batch ops on content providers for now..." )
    }

  private [orm]
  def addRecordSelection( op: Operation, 
                          record: ManagedRecord ) = 
  {
    val mgr = record.id.mgr.asInstanceOf[ BaseRecordManager[_] ]
    val pkey = mgr.primaryKeyField
    val selectionArg: Array[String] = new Array[String](1)
    selectionArg(0) = record.id.id.toString
    addSelection( op, pkey.dbColumnName + "=?", selectionArg )
  }
}
