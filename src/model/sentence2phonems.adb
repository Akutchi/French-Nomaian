with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

package body Sentence2Phonems is

   -----------------------
   -- Get_Raw_Sentences --
   -----------------------

   function Get_Raw_Sentence return Unbounded_Wide_String is

      Sentence : Unbounded_Wide_String := Null_Unbounded_Wide_String;

   begin

      for I in 1 .. Argument_Count loop
         Append (Sentence, To_Wide_String (Argument (I) & " "));
      end loop;

      return Sentence;

   end Get_Raw_Sentence;

   ----------------------------
   -- Get_Accented_Character --
   ----------------------------

   function Get_Accented_Character
     (A_Variant : Wide_Character) return Wide_Character
   is
   begin

      case A_Variant is

         when '¢' =>
            return 'â';

         when '¤' =>
            return 'ä';

         when '®' =>
            return 'î';

         when '¯' =>
            return 'ï';

         when '¹' =>
            return 'ù';

         when '»' =>
            return 'û';

         when '¼' =>
            return 'ü';

         when '©' =>
            return 'é';

         when '¨' =>
            return 'è';

         when 'ª' =>
            return 'ê';

         when '«' =>
            return 'ë';

         when '´' =>
            return 'ô';

         when '¶' =>
            return 'ö';

         when others => -- does not reckognize non-breaking space (?)
            return 'à';

      end case;

   end Get_Accented_Character;

   ---------------
   -- To_French --
   ---------------

   procedure To_French (Sentence : in out Unbounded_Wide_String) is

      Elem      : Wide_Character;
      To_Remove : A_Variant_Index.Vector;
      de        : Natural := 0;
   begin

      if Length (Sentence) /= 0 then

         for I in 1 .. Length (Sentence) - 1 loop

            if Element (Sentence, I) = 'Ã' then

               Elem := Get_Accented_Character (Element (Sentence, I + 1));
               Replace_Element (Sentence, I, Elem);
               A_Variant_Index.Append (To_Remove, I + 1);

            end if;
         end loop;

         if not A_Variant_Index.Is_Empty (To_Remove) then
            for E of To_Remove loop
               Delete (Sentence, E - de, Natural (E - de));
               de := de + 1;
            end loop;
         end if;

      end if;

   end To_French;

end Sentence2Phonems;
