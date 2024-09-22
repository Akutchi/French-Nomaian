with Interfaces.C;
with Ada.Strings.Wide_Unbounded;

package Tui is

   package I_C renames Interfaces.C;
   package S_WU renames Ada.Strings.Wide_Unbounded;

   type Choice_State is record

      Sentence : S_WU.Unbounded_Wide_String := S_WU.Null_Unbounded_Wide_String;
      Quit     : Boolean                    := False;

   end record;

   function Init_Curses return Boolean;

   function Print_Title return Integer;

   function Propose (Y : Integer) return Choice_State;

private

   function Get_Title_Line_Color (I : Natural) return I_C.short;

end Tui;
