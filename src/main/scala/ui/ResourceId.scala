package org.positronicnet.ui

import java.lang.reflect.{Field,Modifier}

object ResourceId {

  private val lock = new Object
  private var nameMap: Map[Int, String] = Map.empty
  private var packagesSeen: Set[String] = Set.empty

  // Test hook... tests are in the 'ui' package so it's visible to them.

  private [ui] def clear = {
    nameMap = Map.empty
    packagesSeen = Set.empty
  }

  def toName (id: Int) = nameMap.get (id)

  def harvestAssociatedResources (thing: Object): Unit = {
    lock.synchronized {
      val klassName = thing.getClass.getName
      val components = klassName.split('.')
      val numPkgComponents = components.size - 1

      def pkgName( numComponents: Int ) = 
        components.slice(0, numComponents).reduce(_ + "." + _)

      val pkg = pkgName( numPkgComponents )

      if (packagesSeen.contains( pkg )) 
        return

      packagesSeen += pkg

      for (i <- 1 to numPkgComponents) {
        try {
          val rKlass = Class.forName( pkgName(i) + ".R" )
          harvestResourcesFromClass( rKlass )
        }
        catch {
          case ex: ClassNotFoundException =>
            // ignore
        }
      }
    }
  }

  private 
  def harvestResourcesFromClass( klass: Class[_] ): Unit = {

    // Have the "R" class.  First, find its "id" member class (or punt),
    // then harvest that for static fields

    val idKlassName = klass.getName + "$id"

    klass.getClasses.find( _.getName == idKlassName ) match {
      case Some(idKlass) => harvestIdFields( idKlass )
      case None => 
        //nothing
    }
  }

  private
  def harvestIdFields( idKlass: Class[_] ): Unit = {
    for (field <- idKlass.getFields) 
      if (Modifier.isStatic (field.getModifiers) 
          && field.getType == java.lang.Integer.TYPE)
        nameMap += (field.getInt(null) -> field.getName)
  }
}
