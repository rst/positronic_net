package org.positronicnet.content

import android.net.Uri
import android.content.ContentProvider
import android.content.ContentValues

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
  import ContentProviderReqType._

  def onCreate: Boolean

  /** Class used internally to represent everything we know about a
    * request, including the `UriMatch` (q.v.) that handles it.
    */

  case class ParsedRequest( reqType:      ContentProviderReqType,
                            uri:          Uri,
                            uriMatchCase: BaseUriMatch,
                            matchValues:  IndexedSeq[ ContentValue ],
                            vals:         ContentValues,
                            where:        String,
                            whereValues:  Array[ String ],
                            cols:         Array[ String ],
                            order:        String )

  // We've got a UriMatcher which has cases for each type of URI we
  // can handle...

  private[this]
  val uriMatcher = new UriMatcher[ BaseUriMatch ]

  private[this]
  def parsedRequestOption( reqType:     ContentProviderReqType,
                           uri:         Uri,
                           where:       String        = null,
                           whereValues: Array[String] = null,
                           cols:        Array[String] = null,
                           vals:        ContentValues = null,
                           order:       String        = null ) =
  {
    uriMatcher.withMatchOption( uri ){ (uriMatchCase, matchValues) =>
      ParsedRequest( reqType, 
                     uri,         uriMatchCase,
                     matchValues, vals,
                     where,       whereValues,
                     cols,        order )
    }
  }

  private[this]
  def parsedRequest( reqType:     ContentProviderReqType,
                     uri:         Uri,
                     where:       String        = null,
                     whereValues: Array[String] = null,
                     cols:        Array[String] = null,
                     vals:        ContentValues = null,
                     order:       String        = null ) = 
  {
    val opt = parsedRequestOption( reqType, uri, where, whereValues, 
                                   cols, vals, order )
    opt.getOrElse{ throw new UnsupportedUri( uri ) }
  }
 
  // Actual operations delegated to the UriMatch (or BaseUriMatch) objects.

  def getType( uri: Uri ) = {
    val opt = parsedRequestOption( GetType, uri )
    opt.map{ _.uriMatchCase.contentType }.getOrElse(null)
  }

  def query( uri: Uri, cols: Array[String], 
             where: String, whereValues: Array[String],
             order: String) = 
  {
    val opt = parsedRequestOption( Query, uri, where, whereValues, 
                                   cols = cols, order = order ) 
    opt match {
      case None => null
      case Some( req ) => req.uriMatchCase.query( req )
    }
  }

  def insert( uri: Uri, vals: ContentValues ) = {
    val req = parsedRequest( Insert, uri, vals = vals )
    req.uriMatchCase.insert( req )
  }

  def delete( uri: Uri, where: String, whereValues: Array[String] ) = {
    val req = parsedRequest( Delete, uri, where, whereValues )
    req.uriMatchCase.delete( req )
  }

  def update( uri: Uri, vals: ContentValues, 
              where: String, whereValues: Array[String] ) = 
  {
    val req = parsedRequest( Update, uri, where, whereValues, vals = vals )
    req.uriMatchCase.update( req )
  }

  def baseNotifyChange( uri: Uri ) = {
    getContext.getContentResolver.notifyChange( uri, null )
  }

  /** Content "subprovider" handling all URIs matching a specific pattern.
    * It has `query`, `insert`, `update` and `delete` methods that will do
    * the obvious (and no more); these may be overridden to add special-case
    * handling.
    * 
    * The pattern is the URI (or string) argument to the constructor;
    * a String will be handed to `Uri.parse`.
    *
    * If using this class directly, you must define `contentType` and
    * `queryForParsedRequest`.  This lets you handle cases where, for
    * whatever reason, you want `queryForParsedRequest` to return
    * different queries based on something other than the match
    * parameters --- for example, adding implicit joins based on
    * what columns are requested.
    *
    * If the return value of your `queryForParsedRequest` would defend
    * only on the matched components of the URI, on the other hand,
    * the more full-featured `UriMatchCase` may let you get the job done
    * with a lot less ceremony.
    */

  abstract class BaseUriMatch( uri: Uri )
  {
    def this( s: String ) = this( Uri.parse( s ))

    uriMatcher.matchUri( uri, this )

    /** Content type of data for queries matching the pattern */

    val contentType: String

    /** Query on our underlying data store for a ParsedRequest */

    def queryForParsedRequest( req: ParsedRequest ): ContentQuery[_,_]

    /** Utility routine to apply the given conditions and order to
      * a `ContentQuery`.  Most implementations of `queryForParsedRequest`
      * will want to call this with some appropriate arguments (possibly
      * massaged from those given in the original request itself.
      */

    protected def augmentQuery( query:       ContentQuery[_,_],
                                where:       String,
                                whereValues: Seq[String],
                                order:       String ) =
    {
      // Apply conditions, if any.

      var augQuery = 
        if (where != null) {
          if (whereValues != null)
            query.where( where, (whereValues.map{ CvString(_) }): _*)
          else
            query.where( where )
        }
        else query

      // Apply order, if any.  (Ignored where it doesn't matter.)

      if (order != null)
        augQuery.order( order )
      else 
        augQuery
    }

    /** ContentValues to actually use in inserts and updates.  Ordinarily,
      * these are exactly those supplied by the client, but this method may
      * be overridden to add parent record IDs, perform safety or
      * consistency checks, etc.
      */

    def contentValues( req: ParsedRequest ) = req.vals

    def query( req: ParsedRequest ) = {
      var query = queryForParsedRequest( req )

      if (req.cols != null)
        query.select( req.cols: _* )
      else
        query.selectDefaultColumns
    }

    def insert( req: ParsedRequest ) = {
      val query   = queryForParsedRequest( req )
      val newId   = query.insertFromContentValues( contentValues( req ))
      val newUri  = Uri.withAppendedPath( req.uri, newId.toString )
      notifyChange( req.uri )           // not newUri?
      newUri
    }

    def update( req: ParsedRequest ) = {
      val query    = queryForParsedRequest( req )
      val res: Int = query.updateFromContentValues( contentValues( req ))
      notifyChange( req.uri )
      res
    }

    def delete( req: ParsedRequest ) = {
      val query    = queryForParsedRequest( req )
      val res: Int = query.delete
      notifyChange( req.uri )
      res
    }

    def notifyChange( uri: Uri ) = baseNotifyChange( uri )
  }

  /** Content "subprovider" handling all URIs matching a URI pattern.
    * This is a special case of `BaseUriMatch` above, which allows
    * for simpler, lower-ceremony handling for common cases.
    *
    * Constructor arguments are:
    *
    *   *) the pattern (URI or string)
    *   *) a sequence of names of columns corresponding to the matched values
    *   *) the contentType for returned rows,
    *   *) a `ContentQuery`
    *
    * For example, consider
    * {{{
    *     new UriMatch( "todo_lists/=/items/=",
    *                   Seq("todo_list_id", "_id"),
    *                   rowContentType( TODO_ITEM_TYPE ),
    *                   TodoDb("todo_items"))
    * }}}
    * For URIs matching this pattern, say, "todo_lists/33/items/45",
    * the query on the underlying content repository will be
    * {{{
    *     TodoDb("todo_items").whereEq( "todo_list_id" -> 33, "_id" -> 45 )
    * }}}
    * Also, for an insert or update operation, the supplied `ContentValues`
    * will have the "todo_list_id" and "_id" columns forced to the values from
    * the URI (so the caller is not required to supply them twice).
    *
    * These behaviors may be changed by overriding the `queryForParsedRequest`
    * and `contentValues` methods, respectively --- although in this case, you
    * might want to just make your own subclass of `BaseUriMatch` instead.
    */

  class UriMatch( 
      uri: Uri,
      columnsMatched: Seq[ String ],
      val contentType: String,
      baseQuery: => ContentQuery[_,_])
    extends BaseUriMatch( uri )
  {
    def this( uriStr: String, columnsMatched: Seq[String],
              contentType: String, baseQuery: => ContentQuery[_,_] ) =
      this( Uri.parse( uriStr ), columnsMatched, contentType, baseQuery )

    /** Default order to use on queries, if not otherwise specified */

    def defaultOrder = null

    def queryForParsedRequest( req: ParsedRequest ): ContentQuery[_,_] = {

      val query = 
        if (columnsMatched != Seq.empty)
          baseQuery.whereEq( columnsMatched.zip( req.matchValues ): _*)
        else
          baseQuery

      val order = if (req.order != null) req.order else defaultOrder

      augmentQuery( query, req.where, req.whereValues, order )
    }
      
    override def contentValues( req: ParsedRequest ) = {

      for (( col, value ) <- columnsMatched.zip( req.matchValues ))
        value.putContentValues( req.vals, col )
      
      req.vals
    }
  }
}

/** Type of a request in a provider's ParsedRequest objects.  One of the
  * distinguished values Insert, Delete, Query, or Update.
  */

class ContentProviderReqType

object ContentProviderReqType {
  case object GetType extends ContentProviderReqType
  case object Insert  extends ContentProviderReqType
  case object Delete  extends ContentProviderReqType
  case object Query   extends ContentProviderReqType
  case object Update  extends ContentProviderReqType
}

/** Somewhat enhanced version of the standard Android `UriMatcher`.
  *
  * In addition to matching string and numeric wildcards, this will
  * collect and give you the values of the matching segments in a URI
  * that matches.
  *
  * One other difference:  a '*' will match any URL segment, as before,
  * but the token for all-digit segments is "=", not "#'.  This is so
  * actual URI objects can be used as patterns; "#" is special syntax
  * in a URI.
  */

class UriMatcher[TMatch] {

  private val basePattern = new PatternNode[ TMatch ]

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

  def matchUri( uriStr: String, matchObj: TMatch):Unit = 
    matchUri( Uri.parse( uriStr ), matchObj )

  def matchUri( uri: Uri, matchObj: TMatch ):Unit = 
  {
    var node = basePattern
    node = node.addNodeForSegment( uri.getAuthority )

    for ( segment <- uri.getPathSegments )
      node = node.addNodeForSegment( segment )

    node.setMatch( matchObj )
  }

}

private [content]
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
        throw new RuntimeException( 
          "Can't mix wildcards and explicit matches in URI patterns" )
    }

  def addNodeForSegment( seg: String ): PatternNode[ TMatch ] = {
    if (seg == "*")
      addWildcard( new StringWildcardArc[ TMatch ])
    else if (seg == "=")
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
      case Some( arcAlreadyThere ) =>
        if (arc.getClass == arcAlreadyThere.getClass) {
          // Two patterns with a same-typed wildcard in the same place.
          // Use the wildcard arc we already have.
          arcAlreadyThere.asInstanceOf[ WildcardPatternArc[TMatch] ].child
        }
        else {
          throw new RuntimeException( 
            "Patterns with different wildcard types following common prefix" )
        }
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
  def toContentValueOpt( s: String ) = {
    if ( s.forall{ java.lang.Character.isDigit(_) } )
      Some( CvLong( java.lang.Long.parseLong( s )))
    else
      None
  }
}

private [content]
class StringWildcardArc[ TMatch ] extends WildcardPatternArc[ TMatch ] {
  def toContentValueOpt( s: String ) = Some( CvString(s) )
}

class UnsupportedUri( uri: Uri )
  extends RuntimeException( "Unsupported URI: " + uri )
