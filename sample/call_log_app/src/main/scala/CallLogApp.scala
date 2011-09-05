package org.positronicnet.sample.call_log

import org.positronicnet.content._
import org.positronicnet.orm._
import org.positronicnet.ui._
import org.positronicnet.notifications._

import android.app.ListActivity
import android.provider.CallLog.Calls
import android.util.Log
import android.view.View
import android.widget.TextView

import java.text.SimpleDateFormat
import java.util.Date

object Resolver extends PositronicContentResolver( "call_log_app" )

case class CallLogEntry( callType:   Int    = 0, 
                         number:     String = null, 
                         cachedName: String = null, 
                         whenRaw:    Long   = 0,
                         id:         Long   = -1 )
  extends ManagedRecord( CallLogEntries )
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
  extends BaseRecordManager[ CallLogEntry ]( 
    Resolver( Calls.CONTENT_URI ).order( Calls.DEFAULT_SORT_ORDER ))
{
  mapField( "callType",   Calls.TYPE )
  mapField( "number",     Calls.NUMBER )
  mapField( "cachedName", Calls.CACHED_NAME )
  mapField( "whenRaw",    Calls.DATE )

  def since( when: Long ) = 
    this.where( Calls.DATE + "> ?", Array( CvLong( when )))
}

class CallLogsAdapter( activity: PositronicActivityHelpers,
                       source: Notifier[IndexedSeq[CallLogEntry]] )
  extends IndexedSeqSourceAdapter( activity, source,
                                   itemViewResourceId = R.layout.call_log_entry)
{
  val dateFormat = new SimpleDateFormat( "yyyy/mm/dd kk:mm" )

  override def bindView( view:View, entry: CallLogEntry ) = {

    def v[ Type ]( id: Int ) = view.findViewById( id ).asInstanceOf[ Type ]

    v[ TextView ]( R.id.call_type ).setText( entry.callTypeName )
    v[ TextView ]( R.id.caller    ).setText( entry.caller )
    v[ TextView ]( R.id.when      ).setText( dateFormat.format( entry.when ))
  }
}

class CallLogActivity extends ListActivity with PositronicActivityHelpers
{
  onCreate {
    useAppFacility( Resolver )
    setContentView( R.layout.call_log_entries )

    // Get last ten days worth of calls.  (Roughly.)

    val limit = System.currentTimeMillis - 10*24*60*60*1000
    setListAdapter( new CallLogsAdapter( this, 
                                         CallLogEntries.since( limit ).records))
  }
}
