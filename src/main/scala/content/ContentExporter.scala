package org.positronicnet.content

import android.net.Uri
import android.content.ContentProvider
import android.content.ContentValues
import android.text.TextUtils

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

/** Augmented version of a standard ContentProvider which handles
  * simple behavior automatically.  (You can elaborate if you like
  * by overriding the default implementations, while still invoking
  * `super` to handle the simple stuff.)
  *
  * In particular, `query`, `update`, `insert` and `delete` calls
  * are mapped automatically to the appropriate operations on a
  * Positronic Net `ContentQuery` returned by the method
  * `getRepository` on the supplied URI.
  *
  * This `getRepository` in turn will apply matches defined by
  * the private `matchUri` method (typically invoked in the
  * constructor).  This presents an augmented version of the
  * standard Android `UriMatcher` which doesn't just note the
  * presence of numeric IDs and text slugs in particular positions,
  * but does something useful with their values.
  */

abstract class PositronicContentProvider
  extends ContentProvider
{
  def onCreate: Boolean

  def getType( uri: Uri ) = 
    getRepoMatchOption( uri ) match {
      case Some( repoMatch ) => repoMatch.contentType
      case None => null
    }

  def query( uri: Uri, cols: Array[String], 
             where: String, whereArgs: Array[String],
             order: String) = 
    getRepoMatchOption( uri ) match {
      case None => null
      case Some( repoMatch ) => {

        var query = addConditions( repoMatch.query, where, whereArgs )

        if (order != null) query = query.order( order )

        if (cols != null)
          query.select( cols: _* )
        else
          query.selectDefaultColumns
      }
    }

  def insert( uri: Uri, vals: ContentValues ) = {
    val newId = getRepository( uri ).insertFromContentValues( vals )
    notifyChange( uri )
    Uri.withAppendedPath( uri, newId.toString )
  }

  def update( uri: Uri, vals: ContentValues, 
              where: String, whereArgs: Array[String] ) = 
  {
    val res: Int = getRepository( uri, where, whereArgs ).updateFromContentValues( vals )
    notifyChange( uri )
    res
  }

  def delete( uri: Uri, vals: ContentValues, 
              where: String, whereArgs: Array[String] ) = 
  {
    val res: Int = getRepository( uri, where, whereArgs ).delete
    notifyChange( uri )
    res
  }

  protected
  def notifyChange( uri: Uri ) = 
    getContext.getContentResolver.notifyChange( uri, null )

  /** Get a ContentQuery for the given uri and query conditions.
    * Defined in terms of the single-argument `getRepository` in the
    * obvious way.
    */

  private[this]
  def getRepository( uri: Uri, where: String, whereArgs: Array[String] )
      : ContentQuery[_,Long] = 
    addConditions( getRepository( uri ), where, whereArgs )

  private[this]
  def addConditions( query: ContentQuery[_,Long],
                     where: String, whereArgs: Array[String] )
    : ContentQuery[_,Long] =
  {
    if (where != null) {
      if (whereArgs != null)
        query.where( where, (whereArgs.map{ CvString(_) }): _*)
      else
        query.where( where )
    }
    else query
  }

  private[this]
  def getRepository( uri: Uri ): ContentQuery[_,Long] = 
    getRepoMatchOption( uri ) match {
      case None => throw new UnsupportedUri( uri )
      case Some( repoMatch ) => repoMatch.query
    }

  private[this]
  val uriMatcher = new UriMatcher[ RepoPattern ]

  private[this]
  def getRepoMatchOption( uri: Uri ): Option[ RepoMatch ] =
    uriMatcher.withMatchOption( uri ){ (repoPattern, vals) =>
      RepoMatch( repoPattern.contentType, repoPattern.completer( vals )) }

  private[this]
  def matchUri( uri: Uri, contentType: String )
              ( func: IndexedSeq[ContentValue] => ContentQuery[_,Long] ) = 
    uriMatcher.matchUri( uri, RepoPattern( contentType, func ))
}

private [content]
case class RepoPattern( 
  contentType: String, 
  completer: IndexedSeq[ContentValue] => ContentQuery[_,Long])

private [content]
case class RepoMatch(
  contentType: String,
  query: ContentQuery[_,Long])

class UriMatcher[TMatch] {

  val basePattern = new PatternNode[ TMatch ]

  def withMatchOption[TRes]
                     (uri: Uri)
                     (func: (TMatch, IndexedSeq[ContentValue]) => TRes)
                  :Option[ TRes ] = 
  {
    val matchValues = new ArrayBuffer[ ContentValue ]
    var patternNodeOpt = basePattern.childForSeg( uri.getAuthority, matchValues)

    for ( segment <- uri.getPathSegments )
      patternNodeOpt = patternNodeOpt.flatMap{ 
        _.childForSeg( segment, matchValues ) }

    patternNodeOpt.flatMap { 
      _.matchOpt.map { 
        matchObj => func( matchObj, matchValues )
      }
    }
  }

  def matchUri( uri: Uri, matchObj: TMatch ):Unit = 
  {
    var node = basePattern
    node = node.addNodeForSegment( uri.getAuthority )

    for ( segment <- uri.getPathSegments )
      node = node.addNodeForSegment( segment )

    node.setMatch( matchObj )
  }

}

class PatternNode[ TMatch ]
{
  def childForSeg(seg: String, vals: ArrayBuffer[ContentValue]) =
    children.flatMap{ _.matchSeg( seg, vals ) }

  private var matchOptInner: Option[ TMatch ] = None

  private var children: Option[ PatternArc[ TMatch ]] = None

  def matchOpt: Option[ TMatch ] = matchOptInner

  def setMatch( matchObj: TMatch ): Unit = 
    matchOptInner match {
      case None =>
        matchOptInner = Some( matchObj )
      case Some( thing ) =>
        throw new RuntimeException( "Duplicate URI pattern definition" )
    }

  def addNodeForSegment( seg: String ): PatternNode[ TMatch ] = {
    if (seg == "*")
      addWildcard( new StringWildcardArc[ TMatch ])
    else if (seg == "#")
      addWildcard( new LongWildcardArc[ TMatch ])
    else {
      children match {
        case None =>
          val arc = new StringMatchArc[ TMatch ]
          children = Some( arc )
          arc.addSeg( seg )
        case Some( arc ) =>
          arc match {
            case sm: StringMatchArc[ TMatch ] =>
              sm.addSeg( seg )
            case _ =>
              throw new RuntimeException( 
                "Can't mix wildcards and explicit matches in URI patterns" )
          }
      }
    }
  }

  def addWildcard( arc: WildcardPatternArc[TMatch] ): PatternNode[TMatch] =
    children match {
      case None =>
        children = Some( arc )
        arc.child
      case Some( arc ) =>
        throw new RuntimeException( 
          "URI segment with both wildcard and explicit match" )
    }
}

private [content]
abstract class PatternArc[ TMatch ]
{
  def matchSeg(seg: String, vals: ArrayBuffer[ContentValue])
       :Option[PatternNode[TMatch]]
}

private [content]
class StringMatchArc[ TMatch ] extends PatternArc[ TMatch ]
{
  private val children = new HashMap[ String, PatternNode[ TMatch ]]
  
  def matchSeg(seg: String, vals: ArrayBuffer[ContentValue]) = 
    children.get( seg )

  def addSeg(seg: String) = 
    children.get( seg ) match {
      case Some( node ) => node
      case None =>
        val node = new PatternNode[ TMatch ]
        children( seg ) = node
        node
    }
}

private [content]
abstract class WildcardPatternArc[ TMatch ] extends PatternArc[ TMatch ]
{
  val child = new PatternNode[ TMatch ]
  
  protected def toContentValueOpt( s: String ): Option[ContentValue]

  def matchSeg( seg: String, vals: ArrayBuffer[ContentValue]) =
    toContentValueOpt( seg ).map { value: ContentValue =>
      vals += value
      child
    }
}

private [content]
class LongWildcardArc[ TMatch ] extends WildcardPatternArc[ TMatch ] {
  def toContentValueOpt( s: String ) = 
    if ( TextUtils.isDigitsOnly( s ))
      Some( CvLong( java.lang.Long.parseLong( s )))
    else
      None
}

private [content]
class StringWildcardArc[ TMatch ] extends WildcardPatternArc[ TMatch ] {
  def toContentValueOpt( s: String ) = Some( CvString(s) )
}

class UnsupportedUri( uri: Uri )
  extends RuntimeException( "Unsupported URI: " + uri )
