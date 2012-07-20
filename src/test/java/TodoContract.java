package org.positronicnet.test;

import android.net.Uri;

// ContentProvider "Contract" declarations for the Todo ContentProvider
// in tests for ContentExporter, etc.

public class TodoContract {

  // URIs for common queries/updates

  public static String TODO_PREFIX = "content://org.positronicnet.test/";
  public static String TODO_LISTS_PREFIX = TODO_PREFIX + "lists";
  public static Uri    TODO_LISTS_URI = Uri.parse( TODO_LISTS_PREFIX );

  public static Uri todoListUri( Long id ) 
  {
    return Uri.withAppendedPath( TODO_LISTS_URI, Long.toString( id ));
  }
    
  public static String TODO_LIST_ITEMS_PATTERN = TODO_LISTS_PREFIX + "/=/items";

  public static Uri todoListItemsUri( Long listId )
  {
    return Uri.withAppendedPath( TODO_LISTS_URI, 
                                 Long.toString( listId ) + "/items" );
  }

  public static String TODO_LIST_ITEM_PATTERN = TODO_LISTS_PREFIX+"/=/items/=";

  public static Uri todoListItemUri( Long listId, Long itemId )
  {
    return Uri.withAppendedPath( TODO_LISTS_URI, 
                                 Long.toString( listId ) + "/items/" + itemId );
  }

  public static String TODO_LIST_TYPE = "vnd.org.positronicnet.todolist";
  public static String TODO_ITEM_TYPE = "vnd.org.positronicnet.todoitem";

  // Columns for individual tables

  static class List {
    public static String ID   = "_id";
    public static String NAME = "name";
  }

  static class Item {
    public static String ID           = "_id";
    public static String TODO_LIST_ID = "todo_list_id";
    public static String DESCRIPTION  = "description";
    public static String IS_DONE      = "is_done";
  }

  // Generating content-types to match platform conventions.
  // Might want these somewhere where they'd be more generally useful

  public static String dirContentType( String s )
  {
    return "vnd.android.cursor.dir/"+s;
  }

  public static String rowContentType( String s )
  {
    return "vnd.android.cursor.item/"+s;
  }
}