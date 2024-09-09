with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

package Phonems2Glyphs is

   LM_Location : constant String := "../src/lm.txt";

   package S_U renames Ada.Strings.Unbounded;

   type GlyphInfo is record

      T         : Character;
      GlyphName : S_U.Unbounded_String;

   end record;

   package List_GlyphInfo is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => GlyphInfo, "=" => "=");

   type String_Tuple is array (Natural range 0 .. 3) of S_U.Unbounded_String;

   package Language_Model is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Wide_String, Element_Type => GlyphInfo,
      Hash     => Ada.Strings.Wide_Hash, Equivalent_Keys => "=");

   function Simplify (Phonems : Wide_String) return Wide_String;

   procedure Init_Language_Model (LM : in out Language_Model.Map);

   function To_Glyphs
     (Phonems : Wide_String; LM : Language_Model.Map)
      return List_GlyphInfo.Vector;

   procedure print_glyphs (List_Glyphs : List_GlyphInfo.Vector);

end Phonems2Glyphs;
