with Ada.Strings;
with Ada.Strings.Wide_Unbounded;
with Ada.Strings.Maps;
with Ada.Strings.Wide_Maps;

with Ada.Characters.Conversions;
with Ada.Text_IO;

with Ada.Containers; use Ada.Containers;

with Locations; use Locations;

package body Phonems2Glyphs is

   package Str renames Ada.Strings;
   package S_WU renames Ada.Strings.Wide_Unbounded;
   package S_M renames Ada.Strings.Maps;
   package S_WM renames Ada.Strings.Wide_Maps;

   package CC renames Ada.Characters.Conversions;
   package IO renames Ada.Text_IO;

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

   ---------------
   -- Construct --
   ---------------

   procedure Construct
     (Spiral : in out Spiral_Model.Tree; GlyphList : List_GlyphInfo.Vector)
   is

      Spiral_Root : constant Spiral_Model.Cursor := Spiral_Model.Root (Spiral);

      Current_V : Spiral_Model.Cursor;
      Current_C : Spiral_Model.Cursor;
      Current_N : Spiral_Model.Cursor;

   begin

      Spiral_Model.Append_Child
        (Spiral, Spiral_Root, ('d', S_U.To_Unbounded_String ("dot_start")));
      --  dot_start because when drawing elements, there's also a "Dot"
      --  function which causes an ambiguity with the GlyphType used to
      --  calculate dx/dy displacement for printing. Also, it litteraly
      --  start the spiral.

      Current_V := Spiral_Model.First_Child (Spiral_Root);
      Current_C := Spiral_Model.First_Child (Spiral_Root);
      Current_N := Spiral_Model.First_Child (Spiral_Root);

      for Node of GlyphList loop

         case Node.T is

            when Vowel =>
               Spiral_Model.Append_Child (Spiral, Current_V, Node);

               Current_V := Spiral_Model.First_Child (Current_V);

            when Numeral =>
               Spiral_Model.Append_Child (Spiral, Current_N, Node);

               Current_N := Spiral_Model.First_Child (Current_N);

            when Consonant | Word_Separator =>

               Spiral_Model.Append_Child (Spiral, Current_C, Node);

               declare
                  Child_Number  : constant Count_Type          :=
                    Spiral_Model.Child_Count (Current_C);
                  C_First_Child : constant Spiral_Model.Cursor :=
                    Spiral_Model.First_Child (Current_C);
               begin

                  Current_C :=
                    (if Child_Number > 1 then
                       Spiral_Model.Next_Sibling (C_First_Child)
                     else C_First_Child);
               end;

            when others =>
               null;

         end case;

         if Node.T = Consonant then
            Current_V := Current_C;

         elsif Node.T = Word_Separator then
            Current_V := Current_C;
            Current_N := Current_C;
         end if;

      end loop;

   end Construct;

   ------------------
   -- Is_Consonant --
   ------------------

   function Is_Consonant (Elem : Spiral_Model.Cursor) return Boolean is
   begin

      if Spiral_Model.Element (Elem).T = Consonant
        or else Spiral_Model.Element (Elem).T = Word_Separator
      then
         return True;
      end if;

      return False;

   end Is_Consonant;

   -----------
   -- Depth --
   -----------

   function Depth
     (Elem : Spiral_Model.Cursor; LM : Language_Model.Map) return Float
   is

      Current_Depth : Float := 0.0;
      Current_Child : Spiral_Model.Cursor;
   begin

      if Is_Consonant (Elem) then
         Current_Depth := Current_Depth + 1.0;
      end if;

      if not Spiral_Model.Is_Leaf (Elem) then

         Current_Child := Spiral_Model.First_Child (Elem);

         while Spiral_Model.Has_Element (Current_Child) loop

            Current_Depth := Current_Depth + Depth (Current_Child, LM);
            Current_Child := Spiral_Model.Next_Sibling (Current_Child);
         end loop;
      end if;

      return Current_Depth;

   end Depth;

   -----------
   -- Print --
   -----------

   procedure Print (List_Glyphs : List_GlyphInfo.Vector) is
   begin

      for E of List_Glyphs loop
         IO.Put (S_U.To_String (E.GlyphName) & "(" & E.T & ") ");
      end loop;

      IO.Put_Line ("");
   end Print;

   procedure Print (Position : Spiral_Model.Cursor) is

      Value : constant GlyphInfo := Spiral_Model.Element (Position);
      Tabs  :
        constant String (1 .. 2 * Integer (Spiral_Model.Depth (Position))) :=
        (others => ' ');
   begin

      IO.Put_Line (Tabs & S_U.To_String (Value.GlyphName));

      if not Spiral_Model.Is_Leaf (Position) then

         Spiral_Model.Iterate_Children (Position, Print'Access);
      end if;

   end Print;

end Phonems2Glyphs;
