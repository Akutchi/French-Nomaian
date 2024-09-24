with Interfaces.C;
with Interfaces.C.Pointers;

package Ncurses_Interface is

   package I_C renames Interfaces.C;

   package wchar_t_Ptr is new Interfaces.C.Pointers
     (Index         => I_C.size_t, Element => I_C.wchar_t,
      Element_Array => I_C.wchar_array, Default_Terminator => I_C.wide_nul);

   function InitScr return I_C.int with
     Import => True, Convention => C, External_Name => "InitScr_Wrp";

   procedure Refresh with
     Import => True, Convention => C, External_Name => "Refresh_Wrp";

   procedure Colored_Line
     (Line : I_C.char_array; Color : I_C.short; y : I_C.int) with
     Import => True, Convention => C, External_Name => "Colored_Line";

   function Menu (Y : I_C.int) return I_C.int with
     Import => True, Convention => C, External_Name => "Menu";

   function Get
     (str_type : I_C.int; Y : I_C.int) return wchar_t_Ptr.Pointer with
     Import => True, Convention => C, External_Name => "Get";

   procedure EndScr with
     Import => True, Convention => C, External_Name => "EndScr_Wrp";

end Ncurses_Interface;
