with Ada.Text_IO;
with Ada.Characters.Latin_1;

with Tui_Constants; use Tui_Constants;
with Locations;     use Locations;

package body Tui is

   package IO renames Ada.Text_IO;
   package CL renames Ada.Characters.Latin_1;

   ------------------
   -- Colored_Line --
   ------------------

   procedure Colored_Line (Line, Color : String) is
   begin

      IO.Put_Line (CL.ESC & Color & Line & CL.ESC & Color);

   end Colored_Line;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (I : Natural) return String is
   begin

      case I is

         when 0 | 1 | 2 =>
            return GOLD;
         when 3 | 4 | 5 | 6 =>
            return RED;
         when 7 | 8 =>
            return GREEN;
         when others =>
            return RESET;

      end case;

   end Get_Color;

   -----------------
   -- Print_Title --
   -----------------

   procedure Print_Title is

      F : IO.File_Type;
      I : Natural := 0;
   begin

      IO.Open (F, IO.In_File, Title_Location);

      while not IO.End_Of_File (F) loop

         declare
            Line  : constant String := IO.Get_Line (F);
            Color : constant String := Get_Color (I);
         begin
            Colored_Line (Line, Color);
         end;

         I := I + 1;

      end loop;

      Colored_Line ("", RESET);

   end Print_Title;

   -------------
   -- Propose --
   -------------

   procedure Propose is
   begin

      IO.Put_Line
        ("Tapper une phrase à traduire (veuillez mettre des guillemets au " &
         "début et à la fin de votre message): ");
      IO.Put ("> ");

   end Propose;

end Tui;
