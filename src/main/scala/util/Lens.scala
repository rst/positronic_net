package org.positronicnet.util

// Crude feasibility study --- 'forType', 'forProperty', and 'implForProperty'
// are all meant to be memoized!

import java.lang.reflect.Field
import java.lang.reflect.Method

@cloneable
class Lensable {

  // Kludge --- attempt to scope package-private access to 'clone',
  // without making it fully public.  But we can't do this in a trait,
  // due to an "implementation restriction".  So, for now, Lensables
  // have a common base class.  Shortly, we use reflection to just
  // get the 'clone' method on 'Object'.  Sigh...

  private[util]
  def duplicate = clone
}

case class Lens[T,V]( 
  get : V,
  set : V => T )

case class LensImpl[T,V](
  getter: T => V,
  setter: (T,V) => T )
{
  def forObject( t: T ) =
    Lens( getter( t ),
          (v:V) => setter( t, v ))
}

abstract class LensFactory[ V : ClassManifest ] {

  def vFromObject( obj: Object ): V
  def vToObject( v: V ): Object
  def vFromField( obj: Object, f: Field ): V
  def vIntoField( obj: Object, f: Field, value: V): Unit

  val targetKlass = classManifest[V].erasure.asInstanceOf[ Class[V] ]

  def implForProperty[ T <: Lensable : ClassManifest ](prop: String): Option[LensImpl[T,V]] = {

    val klass       = classManifest[T].erasure.asInstanceOf[ Class[T] ]

    // First, look for explicit getter/setter pair
    // NB this looks for *public* methods only.

    var getter: Method = null
    var setter: Method = null

    try {
      getter = klass.getMethod( prop )
      setter = klass.getMethod( prop + "_$colon$eq", targetKlass )
    }
    catch {
      case ex: java.lang.NoSuchMethodException =>
        // ... leave them as is.
    }

    if (getter != null && getter.getReturnType.equals( targetKlass ) &&
        setter != null && setter.getReturnType.equals( klass ))
      return Some( LensImpl[T,V]((t:T)     => vFromObject( getter.invoke(t) ),
                                 (t:T,v:V) => {
                                   val vobj = vToObject(v)
                                   setter.invoke(t,vobj).asInstanceOf[T]
                                 }))

    // Failing that, look for a property and do the clone/set thing...

    val fieldOpt = ReflectUtils.declaredFieldsByName( klass ).get( prop )
    
    if (fieldOpt == None) return None   // no luck

    val field = fieldOpt.get

    if (field.getType.equals( targetKlass )) {
      field.setAccessible( true ) // might want some annotation checks...
      return Some( LensImpl[T,V]((t:T) => vFromField( t, field ),
                                 (t:T,v:V) => {
                                   val newT = t.duplicate
                                   vIntoField( newT, field, v )
                                   newT.asInstanceOf[T]
                                 }))
    }
      
                  
    // Wrong type; still no luck.

    return None
  }

  def forProperty[T <: Lensable : ClassManifest](prop: String, t: T) =
    implForProperty[T]( prop ).map{ _.forObject( t ) }
}

// LensFactory for ints.  I'll need one of these for each Java primitive type.

private [util]
object IntLens extends LensFactory[ Int ] {
  def vFromObject( obj: Object ): Int = obj.asInstanceOf[ Integer ].intValue
  def vToObject( v: Int ) = new Integer( v )
  def vFromField( obj: Object, f: Field ) = f.getInt( obj )
  def vIntoField( obj: Object, f: Field, value: Int ) = f.setInt( obj, value )
}

// LensFactory for generic java objects.

private [util]
class ObjectLens[ V : ClassManifest ] extends LensFactory[ V ] {
  def vFromObject( obj: Object ): V = obj.asInstanceOf[ V ]
  def vToObject( v: V ): Object = v.asInstanceOf[ Object ]
  def vFromField( obj: Object, f: Field ) = f.get( obj ).asInstanceOf[ V ]
  def vIntoField( obj: Object, f: Field, value: V ) = 
    f.set( obj, value.asInstanceOf[ Object ] )
}

object LensFactory {
  def forType[ V : ClassManifest ]: LensFactory[V] = {
    val klass = classManifest[V].erasure.asInstanceOf[ Class[V] ]
    if (klass.equals( java.lang.Integer.TYPE ))
      IntLens.asInstanceOf[ LensFactory[ V ]]
    else
      new ObjectLens[ V ]
  }
}
