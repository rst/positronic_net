package org.positronicnet.sample.call_log

import org.positronicnet.content._
import org.positronicnet.orm._
import org.positronicnet.ui._
import org.positronicnet.notifications._
import org.positronicnet.notifications.Actions._

import android.content.Context
import android.util.AttributeSet

import android.app.ListActivity
import android.provider.CallLog.Calls
import android.util.Log
import android.view.View
import android.view.KeyEvent
import android.widget.TextView

import java.text.SimpleDateFormat
import java.util.Date

case class CallLogEntry( 
  callType:   Int    = 0, 
  number:     String = null, 
  cachedName: String = null, 
  whenRaw:    Long   = 0,
  id:         RecordId[CallLogEntry] = CallLogEntries.unsavedId 
  )
  extends ManagedRecord
{
  def callTypeName = callType match {
    case Calls.INCOMING_TYPE => "incoming"
    case Calls.OUTGOING_TYPE => "outgoing"
    case Calls.MISSED_TYPE   => "missed"
    case _                   => "???"
  }

  lazy val caller = if (cachedName == null) number else cachedName
  lazy val when   = new Date( whenRaw )
}

object CallLogEntries
  extends RecordManagerForFields[ CallLogEntry, Calls ]( 
    PositronicContentResolver( Calls.CONTENT_URI ).order( Calls.DEFAULT_SORT_ORDER ))
{
  mapField( "callType", Calls.TYPE ) // override to avoid reserved word 'type'
  mapField( "whenRaw",  Calls.DATE ) // here we're just being different

  // Start with last ten days worth of calls.  (Roughly.)

  val defaultDays:Long = 10
  val millisPerDay = 24*60*60*1000

  lazy val callsWithinLimit = recordsQuery( defaultDays ){ (numDays) => {
    CallLogEntries.where( Calls.DATE + "> ?", 
                          System.currentTimeMillis - numDays * millisPerDay)
  }}
}

class DateTextView( ctx: Context, attrs: AttributeSet )
  extends PositronicTextView( ctx, attrs )

object CallLogsUiBinder extends UiBinder
{
  val dateFormat = new SimpleDateFormat( "yyyy/MM/dd kk:mm" )

  bindProperties[ DateTextView, Date ](
    (view => dateFormat.parse( view.getText.toString )), 
    ((view, date) => view.setText( dateFormat.format( date ))))
}

class CallLogActivity extends ListActivity with PositronicActivityHelpers
{
  lazy val daysDialog = new GetDaysDialog( this )

  onCreate {
    useAppFacility( PositronicContentResolver )
    setContentView( R.layout.call_log_entries )
    setListAdapter( 
      new IndexedSeqSourceAdapter( this, 
                                   CallLogEntries.callsWithinLimit,
                                   R.layout.call_log_entry,
                                   CallLogsUiBinder ))

    useOptionsMenuResource( R.menu.options_menu )
    onOptionsItemSelected( R.id.set_num_days ) { daysDialog.show }
  }
}

class GetDaysDialog( base: ListActivity )
  extends PositronicDialog( base, layoutResourceId = R.layout.days_dialog ) 
{
  def findView[T](tr: TypedResource[T]) = findViewById( tr.id ).asInstanceOf[T]

  val editTxt = findView( TR.dialogEditText )

  editTxt.onKey( KeyEvent.KEYCODE_ENTER ){ doSave; dismiss }
  findView( TR.cancelButton ).onClick { dismiss }
  findView( TR.saveButton ).onClick { doSave; dismiss }
  
  def doSave = {
    val newValue = java.lang.Long.parseLong( editTxt.getText().toString )
    CallLogEntries.callsWithinLimit ! Requery( newValue )
  }

  override def show = { 
    editTxt.setText( CallLogEntries.callsWithinLimit.currentParams.toString )
    super.show
  }
}
