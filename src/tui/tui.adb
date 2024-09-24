with Ada.Text_IO;

with Ncurses_Interface;

with Tui_Constants; use Tui_Constants;
with Locations;     use Locations;

package body Tui is

   package IO renames Ada.Text_IO;

   package N_I renames Ncurses_Interface;

   -----------------
   -- Init_Curses --
   -----------------

   function Init_Curses return Boolean is

      Ret_Value : I_C.int;
   begin

      Ret_Value := N_I.InitScr;

      if Integer (Ret_Value) = -1 then
         return False;
      end if;

      return True;

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

   begin

      IO.Open (F, IO.In_File, Title_Location);

      while not IO.End_Of_File (F) loop

         declare
            Line  : constant I_C.char_array := I_C.To_C (IO.Get_Line (F));
            Color : constant I_C.short      := Get_Title_Line_Color (I);
            Y     : constant I_C.int        := I_C.int (I);
         begin
            N_I.Colored_Line (Line, Color, Y);
         end;

         I := I + 1;

      end loop;

      N_I.Refresh;

      return Integer (I + 1);

   end Print_Title;

   -------------
   -- Propose --
   -------------

   function Propose (Y : Integer) return Choice_State is

      c_Y : constant I_C.int := I_C.int (Y);

      Choosen_Choice : I_C.int;
      Response       : Choice_State;

   begin

      Choosen_Choice := N_I.Menu (c_Y);

      case Choosen_Choice is

         when SENTENCE =>

            declare

               Raw_Ptr : constant N_I.wchar_t_Ptr.Pointer :=
                 N_I.Get (SENTENCE, c_Y);

               N_Raw : constant I_C.ptrdiff_t :=
                 N_I.wchar_t_Ptr.Virtual_Length (Raw_Ptr);

               N : Natural := Natural (N_Raw);

               Wide_Container : Wide_String (1 .. N);

            begin

               I_C.To_Ada (N_I.wchar_t_Ptr.Value (Raw_Ptr), Wide_Container, N);

               Response.Sentence :=
                 S_WU.To_Unbounded_Wide_String (Wide_Container);

            end;

         when FILE_STR =>

            declare

               Raw_Ptr : constant N_I.wchar_t_Ptr.Pointer :=
                 N_I.Get (FILE_STR, c_Y);

               N_Raw : constant I_C.ptrdiff_t :=
                 N_I.wchar_t_Ptr.Virtual_Length (Raw_Ptr);

               N : Natural := Natural (N_Raw);

               Wide_Container : Wide_String (1 .. N);

            begin

               I_C.To_Ada (N_I.wchar_t_Ptr.Value (Raw_Ptr), Wide_Container, N);

               Response.Sentence :=
                 S_WU.To_Unbounded_Wide_String (Wide_Container);

            end;

         when others =>
            Response.Quit := True;
      end case;
      N_I.EndScr;

      return Response;

   end Propose;

end Tui;
