with Ada.Strings.Wide_Unbounded;

package Phonems_Utils is

   package S_WU renames Ada.Strings.Wide_Unbounded;

   Point_Char          : constant Wide_Character := '.';
   Semi_Colon_Char     : constant Wide_Character := ',';
   Comma_Char          : constant Wide_Character := ',';
   Apostrophe_Char     : constant Wide_Character := ''';
   Space_Char          : constant Wide_Character := ' ';
   Word_Separator      : constant Wide_Character := '|';
   Closing_Parenthesis : constant Wide_Character := ')';

   function Is_Integer (word : Wide_String) return Boolean;

   function Is_Float (word : Wide_String) return Boolean;

   function Split_Number (word : Wide_String) return Wide_String;

   function Split_Apostrophe_Word
     (word : Wide_String) return S_WU.Unbounded_Wide_String;

   function Has
     (Char_Pattern : Wide_Character; word : Wide_String) return Boolean;

   function Has_Final_Point (word : Wide_String) return Boolean;

   function Has_Ellipsis (word : Wide_String) return Boolean;

   function Split_Ellipses (word : Wide_String) return Wide_String;

end Phonems_Utils;
