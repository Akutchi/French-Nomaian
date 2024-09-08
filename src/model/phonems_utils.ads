package Phonems_Utils is

   function Get_Accented_Character
     (A_Variant : Wide_Character) return Wide_Character;

   function Is_Integer (word : Wide_String) return Boolean;

   function Is_Float (word : Wide_String) return Boolean;

   function Split_Number (word : Wide_String) return Wide_String;

   function Has_Comma (word : Wide_String) return Boolean;

end Phonems_Utils;
