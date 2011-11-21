package org.positronicnet.util

// Crude feasibility study --- 'forPropertyType', 'forProperty', 
// and 'implForProperty' are all meant to be memoized!

import java.lang.reflect.Field
import java.lang.reflect.Method

@cloneable
trait ReflectiveProperties {

  lazy val selfKlass = this.getClass.asInstanceOf[Class[Object]]

  def getProperty[T : ClassManifest]( prop: String ): T =
    LensFactory.forPropertyType[T].forProperty( selfKlass, prop ).get.getter( this )

  // Note glitch --- return type is ReflectiveProperties, not anything more
  // specific.  So, if you need a more specific type, you either need a cast
  // (as in testPropApi in the specs), or to use lenses directly.

  def setProperty[T : ClassManifest]( prop: String, value: T ): ReflectiveProperties =
    LensFactory.forPropertyType[T].forProperty( selfKlass, prop ).get.setter( this, value ).asInstanceOf[ ReflectiveProperties ]
    
}

case class Lens[T,V](
  getter: T => V,
  setter: (T,V) => T )

abstract class LensFactory[ V : ClassManifest ] {

  def vFromObject( obj: Object ): V
  def vToObject( v: V ): Object
  def vFromField( obj: Object, f: Field ): V
  def vIntoField( obj: Object, f: Field, value: V): Unit

  val targetKlass = classManifest[V].erasure.asInstanceOf[ Class[V] ]

  def forProperty[ T <: ReflectiveProperties : ClassManifest ](prop: String) : Option[Lens[T,V]] =
    this.forProperty( classManifest[T].erasure.asInstanceOf[ Class[T] ], prop )

  private [util]
  def forProperty[ T <: Object ]( klass: Class[T], prop: String ):Option[Lens[T,V]] = {

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
        // ... leave them null
    }

    if (getter != null && getter.getReturnType.equals( targetKlass ) &&
        setter != null && setter.getReturnType.equals( klass ))
      return Some( Lens[T,V]((t:T)     => vFromObject( getter.invoke(t) ),
                             (t:T,v:V) => {
                               val vobj = vToObject( v )
                               setter.invoke( t, vobj ).asInstanceOf[T]
                             }))

    // Failing that, look for a field and try to do the clone/set thing...

    val fieldOpt = ReflectUtils.declaredFieldsByName( klass ).get( prop )
    
    if (fieldOpt == None) return None   // no luck

    val field = fieldOpt.get

    if (field.getType.equals( targetKlass )) {
      field.setAccessible( true ) // might want some annotation checks...
      return Some( Lens[T,V]((t:T) => vFromField( t, field ),
                             (t:T,v:V) => {
                               val newT = LensFactory.klone( t )
                               vIntoField( newT, field, v )
                               newT.asInstanceOf[T]
                             }))
    }
      
                  
    // Wrong type; still no luck.

    return None
  }
}

// LensFactories for Java primitive types.

private [util]
object IntLensFactory extends LensFactory[ Int ] {
  def vFromObject( obj: Object ): Int = obj.asInstanceOf[ Integer ].intValue
  def vToObject( v: Int ) = new Integer( v )
  def vFromField( obj: Object, f: Field ) = f.getInt( obj )
  def vIntoField( obj: Object, f: Field, value: Int ) = f.setInt( obj, value )
}

private [util]
object ByteLensFactory extends LensFactory[ Byte ] {
  def vFromObject( obj: Object ): Byte = obj.asInstanceOf[ Byte ].byteValue
  def vToObject( v: Byte ) = new java.lang.Byte( v )
  def vFromField( obj: Object, f: Field ) = f.getByte( obj )
  def vIntoField( obj: Object, f: Field, value: Byte ) = f.setByte( obj, value )
}

// LensFactory for generic java objects.

private [util]
class ObjectLensFactory[ V : ClassManifest ] extends LensFactory[ V ] {
  def vFromObject( obj: Object ): V = obj.asInstanceOf[ V ]
  def vToObject( v: V ): Object = v.asInstanceOf[ Object ]
  def vFromField( obj: Object, f: Field ) = f.get( obj ).asInstanceOf[ V ]
  def vIntoField( obj: Object, f: Field, value: V ) = 
    f.set( obj, value.asInstanceOf[ Object ] )
}

object LensFactory {

  // We can't access "clone" in a trait the normal way due to
  // an "implementation restriction", so we have this...

  private
  val cloneMethod = classOf[Object].getDeclaredMethod("clone")

  private 
  def klone[T <: Object]( target: T ) = 
    cloneMethod.invoke( target ).asInstanceOf[T]

  cloneMethod.setAccessible( true )

  // All known LensFactories.  The point here is memoization, so if the
  // worst case is that due to some freak timing mistake, two LensFactories
  // for a given property-type come into existence, we can live with it.

  private
  var classToFactory: Map[ Class[_], LensFactory[_] ] = Map.empty

  // compiler glitch if we try to preinitialize like so:
  //
  //    = Map( java.lang.Integer.TYPE -> IntLensFactory,
  //           java.lang.Byte.TYPE    -> ByteLensFactory )

  def forPropertyType[ V : ClassManifest ]: LensFactory[V] = 
    this.forPropertyClass( classManifest[V].erasure.asInstanceOf[ Class[V] ] )

  def forPropertyClass[ V : ClassManifest ]( klass: Class[V] ) =
    classToFactory.get( klass ) match {
      case Some( factory ) => factory.asInstanceOf[ LensFactory[V] ]
      case None => {
        val fac = 
          if (klass.equals( java.lang.Integer.TYPE ))
            IntLensFactory.asInstanceOf[ LensFactory[ V ]]
          else if (klass.equals( java.lang.Byte.TYPE ))
            ByteLensFactory.asInstanceOf[ LensFactory[ V ]]
          else
            new ObjectLensFactory[ V ]

        classToFactory += ( klass -> fac )
        fac
      }
    }
}
