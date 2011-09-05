package org.positronicnet.sample.call_log

import org.positronicnet.content._
import org.positronicnet.orm._
import org.positronicnet.ui._

import android.provider.CallLog.Calls
import android.util.Log

object Resolver extends PositronicContentResolver( "call_log_app" )

case class CallLogEntry( callType:   Int    = 0, 
                         number:     String = null, 
                         cachedName: String = null, 
                         when:       Long   = 0,
                         id:         Long   = -1 )
  extends ManagedRecord( CallLogEntries )

object CallLogEntries
  extends BaseRecordManager[ CallLogEntry ]( 
    Resolver( Calls.CONTENT_URI ).order( Calls.DEFAULT_SORT_ORDER ))
{
  mapField( "callType",   Calls.TYPE )
  mapField( "number",     Calls.NUMBER )
  mapField( "cachedName", Calls.CACHED_NAME )
  mapField( "when",       Calls.DATE )

  def since( when: Long ) = 
    this.where( Calls.DATE + "> ?", Array( CvLong( when )))
}

class CallLogActivity
  extends PositronicActivity
{
  onCreate {
    useAppFacility( Resolver )

    // Get last ten days worth of calls.  (Roughly.)

    val limit = System.currentTimeMillis - 10*24*60*60*1000
    for ( entry <- CallLogEntries.since( limit ).fetchOnThisThread ) 
      Log.d( "call_log_app", entry.toString )
  }
}
