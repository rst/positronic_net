package org.positronicnet.orm

import java.lang.reflect.Constructor
import java.lang.reflect.Field

object ReflectUtils
{
  // Trying to get a constructor for a class.  If there's a no-arg
  // constructor declared, we use that.  Otherwise, if there's exactly
  // *one* constructor declared, and we can get defaults for *all*
  // its arguments, we try for that.

  def getObjectBuilder[T](implicit manifest: ClassManifest[T]) = {
    val klass = manifest.erasure
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
      val args = methods.map{ _.invoke( companionObject ) }
      return (() => constructor.newInstance( args: _* ).asInstanceOf[ T ])
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

  def ancestry( klass: Class[_] ): List[ Class[_]] =
    if (klass == classOf[ AnyRef ]) List( klass )
    else klass :: ancestry( klass.getSuperclass )

  // Getting all values of a particular type from a particular object.
  // Again, technique borrowed from sbt's ReflectUtilities.

  def allVals[T]( obj: AnyRef, klass: Class[T] ): Map[ String, T ] = {

    // Looking for 'lazy val's of the given type.  That's a method and
    // a val with the same name and the right (return) type, with the
    // method taking no arguments.

    val fields = declaredFieldsByName( obj.getClass )

    val candidateMethods = 
      for ( meth <- obj.getClass.getMethods
              if meth.getParameterTypes.length == 0
                 && klass.isAssignableFrom( meth.getReturnType );

            field <- fields.get( meth.getName )
              if field.getType == meth.getReturnType 
          )
        yield meth

    Map( candidateMethods.map{ meth => (meth.getName, 
                                        meth.invoke(obj).asInstanceOf[T]) }:_*)

  }
}
