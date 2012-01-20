package org.positronicnet.sample.contacts

import org.positronicnet.ui._
import org.positronicnet.notifications.Actions._
import org.positronicnet.content.PositronicContentResolver

class EditRawContactActivity
  extends PositronicActivity( layoutResourceId = R.layout.edit_contact )
  with TypedViewHolder
{
  onCreate {
    val rawContact = 
      getIntent.getSerializableExtra( "raw_contact" ).asInstanceOf[ RawContact ]

    rawContact.data ! Fetch { data => 
      val editors = findView( TR.editors )
      for (i <- Range(0, editors.getChildCount)) {
        val editor = editors.getChildAt(i).asInstanceOf[ CategoryDisplay[_] ]
        editor.bind( data )
      }
    }
  }
}
