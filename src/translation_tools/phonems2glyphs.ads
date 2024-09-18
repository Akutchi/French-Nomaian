with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Multiway_Trees;

package Phonems2Glyphs is

   package S_U renames Ada.Strings.Unbounded;

   type String_Tuple is array (Natural range 0 .. 3) of S_U.Unbounded_String;

   Vowel          : constant Character := 'v';
   Consonant      : constant Character := 'c';
   Numeral        : constant Character := 'n';
   Word_Separator : constant Character := 's';
   Starting_Dot   : constant Character := '.';

   type GlyphInfo is record

      T         : Character;
      GlyphName : S_U.Unbounded_String;

   end record;

   package List_GlyphInfo is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => GlyphInfo, "=" => "=");

   package Language_Model is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Wide_String, Element_Type => GlyphInfo,
      Hash     => Ada.Strings.Wide_Hash, Equivalent_Keys => "=");

   package Spiral_Model is new Ada.Containers.Indefinite_Multiway_Trees
     (GlyphInfo, "=");

   function Simplify (Phonems : Wide_String) return Wide_String;

   procedure Init_Language_Model (LM : in out Language_Model.Map);

   function To_Glyphs
     (Phonems : Wide_String; LM : Language_Model.Map)
      return List_GlyphInfo.Vector;

   procedure Construct
     (Spiral : in out Spiral_Model.Tree; GlyphList : List_GlyphInfo.Vector);

   function Max_Depth
     (Elem : Spiral_Model.Cursor; LM : Language_Model.Map) return Float;

   procedure Print (List_Glyphs : List_GlyphInfo.Vector);
   procedure Print (Position : Spiral_Model.Cursor);

private

   function Is_Consonant (Elem : Spiral_Model.Cursor) return Boolean;

end Phonems2Glyphs;
