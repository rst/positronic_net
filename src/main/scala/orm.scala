package org.positronicnet.orm

class Field[ ValType, RecType <: Record ]( name: String, 
                                           rec: RecType, 
                                           optVal: Option[ ValType ] )
{
  def optValue = optVal
  def getName = this.name

  def value = optVal match {
    case Some( x ) => x
    case None => throw new IllegalArgumentException( "unexpectedly null field "+name )
  }

  def apply( l: ValType ): RecType = apply( Some( l ))
  def apply( l: Option[ ValType ] ): RecType = {
    val newRec = rec.getClass.newInstance.asInstanceOf[ RecType ]
    newRec.setAsUpdated( rec, name, l )
    newRec
  }
}

object Field {
  implicit def toValue[V]( f: Field[ V, _ ] ) = f.value
}

class Record 
{
  var inits: Map[ String, Option[ _ ]] = Map.empty

  // Kludge to work around the lack of package-protected constructors

  def setAsUpdated[V]( rec: Record, name: String, opt: Option[V] ): Unit = 
    this.inits = rec.inits.updated( name, opt )

  def field[ValType, T <: Record]( name: String, rec: T ): Field[ ValType, T ]={
    val newValue = this.inits.get( name ) match {
      case Some( opt ) => opt.asInstanceOf[ Option[ ValType ]]
      case None => None
    }
    new Field( name, rec, newValue )
  }

  def longField[ T <: Record ]( name: String, rec: T ) =
    field[ Long, T ]( name, rec )
}

trait RecordManager
  extends Record
{
  val prototypeFields = catalogFields

  // We expect all fields to be declared as 'lazy val's of type
  // Field[ _, _ ].  So, look for that.  (Technique borrowed from
  // sbt's ReflectUtilities, cut down to fit here.)
    
  private def catalogFields: Array[ Field[ _, _ ]] = {
    val klass = this.getClass
    val fieldKlass = classOf[ Field[ _, _ ]]
    val javaFields = declaredFields( klass )

    for (method <- klass.getMethods;
         if ( method.getParameterTypes.length == 0 && 
              fieldKlass.isAssignableFrom( method.getReturnType ));
         field <- javaFields.get( method.getName );
         if field.getType == method.getReturnType)
    yield method.invoke( this ).asInstanceOf[ Field[ _, _ ]]
  }

  private def ancestry( klass: Class[_] ): List[ Class[_]] =
    if (klass == classOf[ AnyRef ]) List( klass )
    else klass :: ancestry( klass.getSuperclass )

  private def declaredFields( klass: Class[_] ) = {
    val fieldList = ancestry( klass ).flatMap( _.getDeclaredFields )
    Map( fieldList.map( f => (f.getName, f )): _* )
  }
}

class Place 
  extends Record 
{
  lazy val latitude  = longField( "latitude", this )
  lazy val longitude = longField( "longitude", this )
}

object Place extends Place with RecordManager
