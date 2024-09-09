with Ada.Strings.Wide_Unbounded;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Wide_Maps;
with Ada.Text_IO;
with Ada.Characters.Conversions;

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Wide_Text_IO;

package body Phonems2Glyphs is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package Str renames Ada.Strings;
   package S_M renames Ada.Strings.Maps;
   package S_WM renames Ada.Strings.Wide_Maps;
   package IO renames Ada.Text_IO;
   package CC renames Ada.Characters.Conversions;

   package W_IO renames Ada.Wide_Text_IO;

   --------------
   -- Simplify --
   --------------

   function Simplify (Phonems : Wide_String) return Wide_String is

      Simplified_Phonems : S_WU.Unbounded_Wide_String :=
        S_WU.Null_Unbounded_Wide_String;

      I : Natural := 1;
      F : Positive;
      L : Natural;

   begin

      while I in Phonems'Range loop

         S_WU.Find_Token
           (Source => S_WU.To_Unbounded_Wide_String (Phonems), From => I,
            Set    => S_WM.To_Set (' '), Test => Str.Outside, First => F,
            Last   => L);

         exit when L = 0;

         if Phonems (F) = Phonems (L) then
            S_WU.Append (Simplified_Phonems, " " & Phonems (F) & " ");

         else
            S_WU.Append (Simplified_Phonems, " " & Phonems (F .. L) & " ");
         end if;

         I := L + 1;

      end loop;

      return S_WU.To_Wide_String (Simplified_Phonems);

   end Simplify;

   ---------------------------
   -- Get_Word_And_Phonetic --
   ---------------------------

   procedure Get_Phonetic_And_Glyph
     (File_Line : String; Dict_Tuple : out String_Tuple)
   is

      k : Natural := 0;

      Index : Natural := 1;
      F     : Positive;
      L     : Natural;

      Whitespace : constant S_M.Character_Set := S_M.To_Set (' ');

   begin

      while Index in File_Line'Range loop

         S_U.Find_Token
           (Source => S_U.To_Unbounded_String (File_Line), Set => Whitespace,
            From   => Index, Test => Str.Outside, First => F, Last => L);

         exit when L = 0;

         Dict_Tuple (k) := S_U.To_Unbounded_String (File_Line (F .. L));

         Index := L + 1;
         k     := k + 1;

      end loop;

   end Get_Phonetic_And_Glyph;

   -------------------------
   -- Init_Language_Model --
   -------------------------

   procedure Init_Language_Model (LM : in out Language_Model.Map) is

      File : IO.File_Type;

      Comment : constant Character := ';';

   begin

      IO.Open (File, IO.In_File, LM_Location);

      while not IO.End_Of_File (File) loop

         declare
            File_Line  : constant String := IO.Get_Line (File);
            Dict_Tuple : String_Tuple;
            GInfo      : GlyphInfo;
         begin

            if File_Line (File_Line'First) /= Comment then

               Get_Phonetic_And_Glyph (File_Line, Dict_Tuple);

               GInfo := (S_U.To_String (Dict_Tuple (2)) (1), Dict_Tuple (1));

               LM.Include
                 (CC.To_Wide_String (S_U.To_String (Dict_Tuple (0))), GInfo);
            end if;
         end;
      end loop;

      IO.Close (File);

   end Init_Language_Model;

   ---------------
   -- To_Glyphs --
   ---------------

   function To_Glyphs
     (Phonems : Wide_String; LM : Language_Model.Map)
      return List_GlyphInfo.Vector
   is
      I : Natural := 1;
      F : Positive;
      L : Natural;

      Whitespace : constant S_WM.Wide_Character_Set := S_WM.To_Set (' ');

      List : List_GlyphInfo.Vector;
   begin

      while I in Phonems'Range loop

         S_WU.Find_Token
           (Source => S_WU.To_Unbounded_Wide_String (Phonems),
            Set    => Whitespace, From => I, Test => Str.Outside, First => F,
            Last   => L);

         exit when L = 0;

         if Language_Model.Contains (LM, Phonems (F .. L)) then
            List_GlyphInfo.Append (List, LM (Phonems (F .. L)));
         else
            IO.Put_Line
              ("Error : " & CC.To_String (Phonems (F .. L)) & " Not in map");
         end if;

         I := L + 1;

      end loop;

      return List;

   end To_Glyphs;

   procedure print_glyphs (List_Glyphs : List_GlyphInfo.Vector) is
   begin

      for E of List_Glyphs loop
         IO.Put (S_U.To_String (E.GlyphName) & "(" & E.T & ") ");
      end loop;

   end print_glyphs;

end Phonems2Glyphs;
