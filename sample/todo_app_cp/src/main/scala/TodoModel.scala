package org.positronicnet.sample.todo_cp

import org.positronicnet.content.PositronicContentResolver
import org.positronicnet.orm._

// Our domain model classes, such as they are:  Todo Items, Lists, etc.
//
// These now model rows as served up by our companion ContentProvider,
// in TodoProvider.scala.  
//
// Note that an item's todo_list_id is not explicitly mapped;
// it's implicit in the URIs used to access the ContentProvider, so the
// ContentProvider will also take care of setting it properly on inserts
// and updates.


case class TodoItem( 
  description: String             = null, 
  isDone:      Boolean            = false,
  id:          RecordId[TodoItem] = TodoItems.unsavedId
) extends ManagedRecord
{
  def setDescription( s: String ) = copy( description = s )
  def setDone( b: Boolean )       = copy( isDone = b )
}

object TodoItems extends 
  DependentRecordManagerForFields[ TodoItem, 
                                   RecordId[TodoList], 
                                   TodoContract.Item](
    (rid: RecordId[TodoList]) => TodoContract.todoListItemsUri( rid.id ))

// "Todo list" model.  

case class TodoList( name: String             = null,
                     id:   RecordId[TodoList] = TodoLists.unsavedId
                   )
  extends ManagedRecord
{
  def setName( s: String ) = copy( name = s )

  lazy val items     = TodoItems.scopeForKey( this.id )
  lazy val doneItems = items.whereEq( "is_done" -> true )
}

object TodoLists extends RecordManagerForFields[ TodoList, TodoContract.List ]( 
  PositronicContentResolver( TodoContract.TODO_LISTS_URI ))

