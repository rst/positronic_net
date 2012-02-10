package org.positronicnet.test.providerspec;

public class Calls
{
  // android.provider.CallLog.Call.CONTENT_URI comes out null in 
  // Robolectric-world, so we have this...

  public static android.net.Uri CONTENT_URI = 
    android.net.Uri.parse("http://org.positronicnet.org/phonycontent/calls");

  public static String NUMBER      = "number";
  public static String CACHED_NAME = "cached_name";
  public static String DATE        = "date";
  public static String TYPE        = "type";
  public static String _ID         = "_id";
}

