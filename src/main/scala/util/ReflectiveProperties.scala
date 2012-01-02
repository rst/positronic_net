package org.positronicnet.util

import java.lang.reflect.Field
import java.lang.reflect.Method

@cloneable
trait ReflectiveProperties {

  lazy val selfKlass = this.getClass.asInstanceOf[Class[Object]]

  def getProperty[T : ClassManifest]( prop: String ): T =
    PropertyLensFactory.forPropertyType[T].forProperty( selfKlass, prop ).get.getter( this )

  def setProperty[T : ClassManifest]( prop: String, value: T ): this.type =
    PropertyLensFactory.forPropertyType[T].forProperty( selfKlass, prop ).get.setter( this, value ).asInstanceOf[ this.type ]
    
}

case class PropertyLens[T,V](
  getter: T => V,
  setter: (T,V) => T,
  hostClass: Class[T],
  valueClass: Class[V]
)

abstract class PropertyLensFactory[ V : ClassManifest ] {

  def vFromObject( obj: Object ): V
  def vToObject( v: V ): Object
  def vFromField( obj: Object, f: Field ): V
  def vIntoField( obj: Object, f: Field, value: V): Unit

  private
  val valueKlass = classManifest[V].erasure.asInstanceOf[ Class[V] ]

  private
  var lensMap: Map[(Class[_], String), Option[PropertyLens[_,_]]] = Map.empty

  def forProperty[ T <: ReflectiveProperties : ClassManifest ](prop: String)
         :Option[ PropertyLens[T,V]] =
    this.forProperty( classManifest[T].erasure.asInstanceOf[ Class[T] ], prop )

  def forProperty[T <: Object](klass: Class[T], prop: String)
         :Option[ PropertyLens[T,V]] =
  {
    val key = (klass, prop)
    lensMap.get( key ) match {
      case Some(option) => option.asInstanceOf[Option[PropertyLens[T,V]]]
      case None =>
        val lensOption = this.buildLens( klass, prop )
        lensMap += (key -> lensOption)
        lensOption
    }
  }

  def buildLens[ T <: Object ]( klass: Class[T], prop: String ):Option[PropertyLens[T,V]] = {
    // First, look for explicit getter/setter pair
    // NB this looks for *public* methods only.

    var getter: Method = null
    var setter: Method = null

    try {
      getter = klass.getMethod( prop )
      setter = klass.getMethod( prop + "_$colon$eq", valueKlass )
    }
    catch {
      case ex: java.lang.NoSuchMethodException =>
        // ... leave them null
    }

    if (getter != null && getter.getReturnType.equals( valueKlass ) &&
        setter != null && setter.getReturnType.equals( klass ))
      return Some( PropertyLens[T,V]((t)   => vFromObject( getter.invoke(t) ),
                                     (t,v) => {
                                       val vobj = vToObject( v )
                                       setter.invoke( t, vobj ).asInstanceOf[T]
                                     },
                                     klass,
                                     valueKlass
                                   ))

    // Failing that, look for a field and try to do the clone/set thing...

    val fieldOpt = ReflectUtils.declaredFieldsByName( klass ).get( prop )
    
    if (fieldOpt != None) {

      val field = fieldOpt.get

      if (field.getType.equals( valueKlass )) {
        field.setAccessible( true ) // might want some annotation checks...
        return Some( PropertyLens[T,V]((t) => vFromField( t, field ),
                                       (t,v) => {
                                         val newT = PropertyLensFactory.klone(t)
                                         vIntoField( newT, field, v )
                                         newT.asInstanceOf[T]
                                       },
                                       klass,
                                       valueKlass
                                     ))
      }
    }
      
    // Failing *that*, if we managed to extract a correctly-typed 'getter',
    // treat as a read-only property.

    if (getter != null && getter.getReturnType.equals( valueKlass )) {
      return Some( PropertyLens[T,V]((t) => vFromObject( getter.invoke(t) ),
                                     (t, v) => 
                                       throw new ReadOnlyProperty(klass,prop),
                                     klass,
                                     valueKlass
                                    ))
    }
                  
    // Nope, got nothing.

    return None
  }
}

// PropertyLensFactory singleton object --- mostly a way to instantiate 'em
// (note the definitions of special-case lens factories for primitive
// types below), but also has some runtime support for them.

object PropertyLensFactory {

  // We can't access "clone" in a trait the normal way due to
  // an "implementation restriction", so we have this...

  private
  val cloneMethod = classOf[Object].getDeclaredMethod("clone")

  cloneMethod.setAccessible( true )

  private 
  def klone[T <: Object]( target: T ) = 
    cloneMethod.invoke( target ).asInstanceOf[T]

  // All known LensFactories.  The point here is memoization, so if the
  // worst case is that due to some freak timing mistake, two LensFactories
  // for a given property-type come into existence, we can live with it.

  private
  var classToFactory: Map[ Class[_], PropertyLensFactory[_] ] = Map.empty

  // compiler glitch if we try to preinitialize like so:
  //
  //    = Map( java.lang.Integer.TYPE -> IntLensFactory,
  //           java.lang.Byte.TYPE    -> ByteLensFactory )

  def forPropertyType[ V : ClassManifest ]: PropertyLensFactory[V] = 
    this.forPropertyClass( classManifest[V].erasure.asInstanceOf[ Class[V] ] )

  def forPropertyClass[ V : ClassManifest ]( klass: Class[V] ) =
    classToFactory.get( klass ) match {
      case Some( factory ) => factory.asInstanceOf[ PropertyLensFactory[V] ]
      case None => {
        val fac = 
          if (klass.equals( java.lang.Integer.TYPE ))
            IntLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Byte.TYPE ))
            ByteLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Character.TYPE ))
            CharLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Short.TYPE ))
            ShortLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Long.TYPE ))
            LongLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Float.TYPE ))
            FloatLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Double.TYPE ))
            DoubleLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else if (klass.equals( java.lang.Boolean.TYPE ))
            BooleanLensFactory.asInstanceOf[ PropertyLensFactory[ V ]]
          else
            new ObjectLensFactory[ V ]

        classToFactory += ( klass -> fac )
        fac
      }
    }
}

// PropertyLensFactory for generic java objects.

private [util]
class ObjectLensFactory[ V : ClassManifest ] extends PropertyLensFactory[ V ] {
  def vFromObject( obj: Object ): V = obj.asInstanceOf[ V ]
  def vToObject( v: V ): Object = v.asInstanceOf[ Object ]
  def vFromField( obj: Object, f: Field ) = f.get( obj ).asInstanceOf[ V ]
  def vIntoField( obj: Object, f: Field, value: V ) = 
    f.set( obj, value.asInstanceOf[ Object ] )
}
// LensFactories for Java primitive types.

private [util]
object IntLensFactory extends PropertyLensFactory[ Int ] {
  def vFromObject( obj: Object ): Int = obj.asInstanceOf[ Integer ].intValue
  def vToObject( v: Int ) = new Integer( v )
  def vFromField( obj: Object, f: Field ) = f.getInt( obj )
  def vIntoField( obj: Object, f: Field, value: Int ) = f.setInt( obj, value )
}

private [util]
object ByteLensFactory extends PropertyLensFactory[ Byte ] {
  def vFromObject( obj: Object ): Byte = obj.asInstanceOf[ Byte ].byteValue
  def vToObject( v: Byte ) = new java.lang.Byte( v )
  def vFromField( obj: Object, f: Field ) = f.getByte( obj )
  def vIntoField( obj: Object, f: Field, value: Byte ) = f.setByte( obj, value )
}

private [util]
object CharLensFactory extends PropertyLensFactory[ Char ] {
  def vFromObject( obj: Object ): Char = obj.asInstanceOf[ Char ].charValue
  def vToObject( v: Char ) = new java.lang.Character( v )
  def vFromField( obj: Object, f: Field ) = f.getChar( obj )
  def vIntoField( obj: Object, f: Field, value: Char ) = f.setChar( obj, value )
}

private [util]
object ShortLensFactory extends PropertyLensFactory[ Short ] {
  def vFromObject( obj: Object ): Short = obj.asInstanceOf[ Short ].shortValue
  def vToObject( v: Short ) = new java.lang.Short( v )
  def vFromField( obj: Object, f: Field ) = f.getShort( obj )
  def vIntoField( obj: Object, f: Field, value: Short ) = f.setShort(obj, value)
}

private [util]
object LongLensFactory extends PropertyLensFactory[ Long ] {
  def vFromObject( obj: Object ): Long = obj.asInstanceOf[ Long ].longValue
  def vToObject( v: Long ) = new java.lang.Long( v )
  def vFromField( obj: Object, f: Field ) = f.getLong( obj )
  def vIntoField( obj: Object, f: Field, value: Long ) = f.setLong( obj, value )
}

private [util]
object FloatLensFactory extends PropertyLensFactory[ Float ] {
  def vFromObject( obj: Object ): Float = obj.asInstanceOf[ Float ].floatValue
  def vToObject( v: Float ) = new java.lang.Float( v )
  def vFromField( obj: Object, f: Field ) = f.getFloat( obj )
  def vIntoField( obj: Object, f: Field, value: Float ) = 
    f.setFloat( obj, value )
}

private [util]
object DoubleLensFactory extends PropertyLensFactory[ Double ] {
  def vFromObject( obj: Object ): Double = 
    obj.asInstanceOf[ Double ].doubleValue
  def vToObject( v: Double ) = new java.lang.Double( v )
  def vFromField( obj: Object, f: Field ) = f.getDouble( obj )
  def vIntoField( obj: Object, f: Field, value: Double ) = 
    f.setDouble( obj, value )
}

private [util]
object BooleanLensFactory extends PropertyLensFactory[ Boolean ] {
  def vFromObject( obj: Object ): Boolean = 
    obj.asInstanceOf[ Boolean ].booleanValue
  def vToObject( v: Boolean ) = new java.lang.Boolean( v )
  def vFromField( obj: Object, f: Field ) = f.getBoolean( obj )
  def vIntoField( obj: Object, f: Field, value: Boolean ) = 
    f.setBoolean( obj, value )
}

class ReadOnlyProperty( klass: Class[_], prop: String )
  extends RuntimeException( "Attempt to set read-only property " + prop + 
                            " of class " + klass.toString )
