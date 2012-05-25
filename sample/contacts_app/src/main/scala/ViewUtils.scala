package org.positronicnet.sample.contacts

import org.positronicnet.ui._

import android.app.Activity
import android.view.{View, ViewGroup}
import android.content.{Context, ContextWrapper, Intent}

import android.app.AlertDialog
import android.content.DialogInterface  // android.content?!

import android.util.Log

import scala.collection.mutable.ArrayBuffer

// Class which actually represents the cases in a "dialogResultMatch"

case class DialogCase( titleResource: Int, handler: () => Unit )

trait WidgetUtils extends View with ViewUtils with PositronicHandlers {
  def dialogResultMatch( titleRes: Int )( cases: DialogCase* ) =
    dialogResultMatchFromContext( this.getContext, titleRes )( cases: _* )
}

trait ActivityViewUtils extends Activity with ViewUtils {
  def dialogResultMatch( titleRes: Int )( cases: DialogCase* ) =
    dialogResultMatchFromContext( this, titleRes )( cases: _* )
}

trait ViewUtils extends org.positronicnet.ui.GenericViewUtils {
  def dialogResultMatchFromContext( context: Context, titleRes: Int )
                                  ( cases: DialogCase* ) = 
  {
    val res = context.getResources
    val caseToString = (( dc: DialogCase ) => res.getString( dc.titleResource ))
    val casesISeq = cases.toIndexedSeq
    withChoiceFromDialogInContext( context, titleRes, casesISeq, caseToString ){
      _.handler()
    }
  }

  def dialogCase( titleResource: Int )( handler: => Unit ) =
    DialogCase( titleResource, () => handler )
}

