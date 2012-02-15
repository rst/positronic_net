package org.positronicnet.sample.contacts

import org.positronicnet.ui._

import android.app.Activity
import android.view.{View, ViewGroup}
import android.content.Context

import android.app.AlertDialog
import android.content.DialogInterface  // android.content?!

import android.util.Log

import scala.collection.mutable.ArrayBuffer

trait WidgetUtils extends View with ViewUtils {

  def parentOfType[ ViewType <: View : ClassManifest ]: ViewType = 
    parentOfType[ ViewType ]( this )

  def childrenOfType[ ViewType <: View : ClassManifest ]: IndexedSeq[ViewType] =
    childrenOfType[ ViewType ]( this )

  def withChoiceFromDialog[T](titleRes: Int, 
                              vals: IndexedSeq[T], 
                              labeler: T => String)
                             (handler: T => Unit): Unit = 
    withChoiceFromDialogInContext( this.getContext, titleRes, 
                                   vals, labeler )( handler )
}

trait ActivityViewUtils extends Activity with ViewUtils {

  def withChoiceFromDialog[T](titleRes: Int, 
                              vals: IndexedSeq[T], 
                              labeler: T => String)
                             (handler: T => Unit): Unit = 
    withChoiceFromDialogInContext( this, titleRes, vals, labeler )( handler )
}

trait ViewUtils {

  def parentOfType[ ViewType <: View : ClassManifest ]( view: View ) = {

    val targetKlass = classManifest[ ViewType ].erasure
    var parent = view.getParent

    while (parent != null && !targetKlass.isInstance( parent )) {
      parent = parent.getParent
    }

    parent.asInstanceOf[ ViewType ]
  }

  def childrenOfType[ V <: View : ClassManifest ]( root: View ):IndexedSeq[V]={
    val accum = new ArrayBuffer[V]
    collectChildren( root, classManifest[V].erasure, accum )
    accum
  }

  private 
  def collectChildren[V](root: View, klass: Class[_], buf: ArrayBuffer[V]):Unit=
    if (klass.isInstance( root ))
      buf += root.asInstanceOf[V]
    else
      root match {
        case grp: ViewGroup =>
          for (i <- Range( 0, grp.getChildCount() ))
            collectChildren( grp.getChildAt(i), klass, buf )
        case _ =>
      }

  def withChoiceFromDialogInContext[T](context: Context,
                                       titleRes: Int, 
                                       vals: IndexedSeq[T], 
                                       labeler: T => String)
                                      (handler: T => Unit) = 
  {
    val dbuilder = new AlertDialog.Builder( context )
    dbuilder.setTitle( titleRes )
    dbuilder.setItems( 
      vals.map{ labeler(_).asInstanceOf[CharSequence] }.toArray,
      new DialogInterface.OnClickListener {
        def onClick( dialog: DialogInterface, idx: Int ) = {
          handler( vals( idx ) )
        }
      }
    )
    dbuilder.create.show
  }
}

