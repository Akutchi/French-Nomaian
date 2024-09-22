with Interfaces.C;

package Tui is

   package I_C renames Interfaces.C;

   function Init_Curses return Integer;

   function Print_Title return Integer;

   procedure Propose (Y : Integer);

private

   function Get_Title_Line_Color (I : Natural) return I_C.short;

end Tui;
