with Ada.Strings.Wide_Unbounded;

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Hash;

package Sentence2Phonems is

   package S_WU renames Ada.Strings.Wide_Unbounded;

   Cmudict_Location : String :=
     "/home/akutchi/Desktop/Ada_programs/french_nomaian/src/cmudict_fr.txt";

   package Latin_Base_Companion_Char_Index is new Ada.Containers
     .Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Positive);

   package Cmudict is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Wide_String, Element_Type => Wide_String,
      Hash     => Ada.Strings.Wide_Hash, Equivalent_Keys => "=");

   type Wide_Tuple is
     array (Natural range 0 .. 1) of S_WU.Unbounded_Wide_String;

   function Get_Raw_Sentence return S_WU.Unbounded_Wide_String;

   procedure To_French (Sentence : in out S_WU.Unbounded_Wide_String);
   --  Does not handle caps such as " À "

   function To_Phonems
     (Sentence : S_WU.Unbounded_Wide_String) return Wide_String;
   --  I don't check for caps. Thus, you CoulD WRiTe LiKe THiS and it wouldn't
   --  care.

private

   procedure Get_Word_And_Phonetic
     (File_Line : Wide_String; Dict_Tuple : out Wide_Tuple);

   function Is_Not_Phonetic_Variant
     (Word : S_WU.Unbounded_Wide_String) return Boolean;
   --  If not of the form XXXX(k)

   procedure Init_Cmudict (dict : in out Cmudict.Map);

   procedure Decide_On_Word
     (Phonems : in out S_WU.Unbounded_Wide_String; dict : Cmudict.Map;
      word    :        Wide_String);

end Sentence2Phonems;
