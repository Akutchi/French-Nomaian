with Interfaces.C;
with Interfaces.C.Strings;

package Ncurses_Interface is

   package I_C renames Interfaces.C;
   package I_CS renames Interfaces.C.Strings;

   function InitScr return I_C.int with
     Import => True, Convention => C, External_Name => "InitScr_Wrp";

   procedure Refresh with
     Import => True, Convention => C, External_Name => "Refresh_Wrp";

   procedure Colored_Line
     (Line : I_C.char_array; Color : I_C.short; y : I_C.int) with
     Import => True, Convention => C, External_Name => "Colored_Line";

   function Menu (Y : I_C.int) return I_C.int with
     Import => True, Convention => C, External_Name => "Menu";

   function Get (str_type : I_C.int; Y : I_C.int) return I_CS.chars_ptr with
     Import => True, Convention => C, External_Name => "Get";

   procedure EndScr with
     Import => True, Convention => C, External_Name => "EndScr_Wrp";

end Ncurses_Interface;
