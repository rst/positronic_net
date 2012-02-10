package org.positronicnet.util

import java.lang.reflect.Constructor
import java.lang.reflect.Field
import java.lang.reflect.Modifier

object ReflectUtils
{
  // Trying to get a constructor for a class.  If there's a no-arg
  // constructor declared, we use that.  Otherwise, if there's exactly
  // *one* constructor declared, and we can get defaults for *all*
  // its arguments, we try for that.

  def getObjectBuilder[T](implicit manifest: ClassManifest[T]) = 
    getObjectBuilderForClass( manifest.erasure ).asInstanceOf[(() => T)]

  def getObjectBuilderForClass[T](klass: Class[T]) = {
    val constructors = klass.getConstructors
    constructors.find{ _.getParameterTypes.size == 0 } match {
      case Some( constructor ) => 
        (() => constructor.newInstance().asInstanceOf[T])
      case None => 
        getDefaultingObjectBuilder( klass, constructors )
    }
  }

  private
  def getDefaultingObjectBuilder[T]( klass: java.lang.Class[_],
                                     constructors: Array[ Constructor[_] ] ):
    (() => T) =
  {
    if (constructors.length != 1) {

      // If more than one constructor exists, I'm not sure how to identify
      // which one takes the defaults from reflection, so for now, we punt.

      throw new 
      IllegalArgumentException( "Can't identify constructor for " 
                                + klass.getName + 
                                "; more than one, and all require arguments" )
    }

    try {
      // If there's ONE constructor, and ALL arguments are defaulted,
      // we try to use that.

      val constructor = constructors(0)
      val constructorParmTypes = constructor.getParameterTypes
      val companionClass = Class.forName( klass.getName + "$" )
      val methods = 
        for (i <- Range( 1, constructorParmTypes.size + 1 ))
        yield companionClass.getDeclaredMethod("init$default$"+i.toString)
      val companionObject = companionClass.getField("MODULE$").get(null)
      return (() => {
        val args = methods.map{ _.invoke( companionObject ) }
        constructor.newInstance( args: _* ).asInstanceOf[ T ]
      })
    }
    catch {
      case ex: java.lang.NoSuchMethodException =>
        throw new IllegalArgumentException( "Can't find no-arg constructor for "
                                            + klass.getName )
    }
  }

  // Extracting Field objects for all declared fields of a class.
  // Technique borrowed from sbt's ReflectUtilities, cut down to fit here.

  def declaredFieldsByName( klass: Class[_] ) = {
    val fieldList = ancestry( klass ).flatMap( _.getDeclaredFields )
    Map( fieldList.map( f => (f.getName, f )): _* )
  }

  // Extracting public static values of a given Java type from a Java class.
  // (Android content providers often have column names of static fields
  // defined as static fields on some class, e.g., CallLog.Calls.  The ORM
  // uses this to fish out the names and associated values.)

  def publicStaticValues[T]( valueKlass: Class[T], 
                             srcKlass: Class[_] ):Map[String,T] = 
  {
    val ourFields = srcKlass.getFields.filter { f =>
      Modifier.isStatic( f.getModifiers ) && f.getType == valueKlass }
    val map = new scala.collection.mutable.HashMap[ String, Field ]

    for (f <- ourFields) {
      map.get( f.getName ) match {
        case None => 
          map( f.getName ) = f
        case Some( ff ) => {
          if (ff.getDeclaringClass.isAssignableFrom( f.getDeclaringClass )) {
            // replace with field from more specific type
            map( f.getName ) = f
          }
          else if (f.getDeclaringClass.isAssignableFrom(ff.getDeclaringClass)) {
            // already had more specific type; do nothing
          }
          else {
            android.util.Log.d( 
              "XXX", 
              valueKlass.getName+" has ambiguous definition for "+f.getName )
          }
        }
      }
    }
    
    map.mapValues( _.get(null).asInstanceOf[T] ).toMap
  }

  // Extracting public static values of a given Java type from a Java class,
  // expressed using Scala type notation.
  //
  // (Android content providers often have column names of static fields
  // defined as static fields on some class, e.g., CallLog.Calls.  The ORM
  // uses this to fish out the names and associated values.)

  def getStatics[ TVal: ClassManifest, TSrc: ClassManifest ] = 
    publicStaticValues[ TVal ]( 
      classManifest[ TVal ].erasure.asInstanceOf[ Class[ TVal ]],
      classManifest[ TSrc ].erasure )

  // Extracting value of a single public static field

  def getStatic[ TVal: ClassManifest, TSrc: ClassManifest ](fieldName: String)={
    val srcKlass = classManifest[ TSrc ].erasure
    val valKlass = classManifest[ TVal ].erasure
    val fld = srcKlass.getField( fieldName )
    if ( Modifier.isStatic( fld.getModifiers ) && fld.getType == valKlass )
      fld.get(null).asInstanceOf[ TVal ]
    else
      throw new RuntimeException( srcKlass.toString + " has no public static "+ 
                                  valKlass.toString + " " + fieldName )
  }

  // Returns a list of the argument class and all its superclasses

  def ancestry( klass: Class[_] ): List[ Class[_]] =
    if (klass == classOf[ AnyRef ]) List( klass )
    else klass :: ancestry( klass.getSuperclass )

  // Getting a function to get all values of a particular type from
  // objects of a given class.  (For example, getting all HasManyAssociations
  // from a ManagedRecord.)
  //
  // Returns an Option --- a function to get the values if the target
  // class has any, otherwise None.
  //
  // Again, technique borrowed from sbt's ReflectUtilities.

  def extractor[R,T]( targetKlass: Class[R], klass: Class[T] ): 
    Option[ R => Map[ String, T ]] = 
  {
    // Looking for 'lazy val's of the given type.  That's a method and
    // a val with the same name and the right (return) type, with the
    // method taking no arguments.

    val fields = declaredFieldsByName( targetKlass )

    val candidateMethods = 
      for ( meth <- targetKlass.getMethods
              if meth.getParameterTypes.length == 0
                 && klass.isAssignableFrom( meth.getReturnType );

            field <- fields.get( meth.getName )
              if field.getType == meth.getReturnType 
          )
        yield meth

    if (candidateMethods.size == 0)
      return None
    else
      return Some( 
        x =>
          Map(candidateMethods.map{ meth => (meth.getName, 
                                             meth.invoke(x).asInstanceOf[T])}:_*
             ))

  }
}
