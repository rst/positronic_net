package org.positronicnet.sample.call_log

import org.positronicnet.content._
import org.positronicnet.ui._

import android.provider.CallLog.Calls
import android.util.Log

case class CallLogEntry( callType: Int, number: String, 
                         cachedName: String, when: Long )

object CallLog
  extends PositronicContentResolver( "call_log_app" )
{
  val src = this( Calls.CONTENT_URI.toString ).order(Calls.DEFAULT_SORT_ORDER)

  def since( when: Long ) = {
    val qualified_src = src.where( Calls.DATE + ">" + when )
    for ( c <- qualified_src.select( Calls.TYPE, Calls.NUMBER, 
                                     Calls.CACHED_NAME, Calls.DATE ))
    yield CallLogEntry( c.getInt( 0 ), c.getString( 1 ), c.getString( 2 ),
                        c.getLong( 3 ))
  }
}

class CallLogActivity
  extends PositronicActivity
{
  onCreate {
    useAppFacility( CallLog )

    // Get last ten days worth of calls.  (Roughly.)
    val stuff = CallLog.since( System.currentTimeMillis - 10*24*60*60*1000 )
    for ( thing <- stuff ) Log.d( "call_log_app", thing.toString )
  }
}
