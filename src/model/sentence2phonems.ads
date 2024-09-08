with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Sentence2Phonems is

   package A_Variant_Index is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Positive);

   function Get_Raw_Sentence return Unbounded_Wide_String;

   procedure To_French (Sentence : in out Unbounded_Wide_String);
   -- Does not handle caps such as " À "

private

   function Get_Accented_Character
     (A_Variant : Wide_Character) return Wide_Character;

end Sentence2Phonems;
