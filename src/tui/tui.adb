with Ada.Text_IO;

with Tui_Ncurses;

with Tui_Constants; use Tui_Constants;
with Locations;     use Locations;

package body Tui is

   package IO renames Ada.Text_IO;
   package T_N renames Tui_Ncurses;

   -----------------
   -- Init_Curses --
   -----------------

   function Init_Curses return Integer is

      Ret_Value : I_C.int;
   begin

      Ret_Value := T_N.InitScr;

      return Integer (Ret_Value);

   end Init_Curses;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Title_Line_Color (I : Natural) return I_C.short is
   begin

      case I is

         when L1 | L2 | L3 =>
            return GOLD;
         when L4 | L5 | L6 =>
            return RED;
         when L7 | L8 =>
            return GREEN;
         when others =>
            return RESET;

      end case;

   end Get_Title_Line_Color;

   -----------------
   -- Print_Title --
   -----------------

   function Print_Title return Integer is

      F : IO.File_Type;
      I : Positive := 1;

      End_Line : constant I_C.char_array := I_C.To_C ("");
   begin

      IO.Open (F, IO.In_File, Title_Location);

      while not IO.End_Of_File (F) loop

         declare
            Line  : constant I_C.char_array := I_C.To_C (IO.Get_Line (F));
            Color : constant I_C.short      := Get_Title_Line_Color (I);
            Y     : constant I_C.int        := I_C.int (I);
         begin
            T_N.Colored_Line (Line, Color, Y);
         end;

         I := I + 1;

      end loop;

      T_N.Refresh;

      return Integer (I + 1);

   end Print_Title;

   -------------
   -- Propose --
   -------------

   procedure Propose (Y : Integer) is

      Choosen_Choice : I_C.int;
      c_Y            : constant I_C.int := I_C.int (Y);

   begin

      Choosen_Choice := T_N.Menu (c_Y);
      T_N.EndScr;

   end Propose;

end Tui;
