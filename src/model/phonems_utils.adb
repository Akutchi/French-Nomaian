with Ada.Characters.Conversions;

package body Phonems_Utils is

   package CC renames Ada.Characters.Conversions;

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

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (word : Wide_String) return Boolean is

      d : Integer;
   begin

      begin
         d := Integer'Value (CC.To_String (word));

      exception
         when others =>
            return False;

      end;

      return True;

   end Is_Integer;

   --------------
   -- Is_Float --
   --------------

   function Is_Float (word : Wide_String) return Boolean is

      f : Float;
   begin

      begin
         f := Float'Value (CC.To_String (word));

      exception
         when others =>
            return False;

      end;

      return True;

   end Is_Float;

   ------------------
   -- Split_Number --
   ------------------

   function Split_Number (word : Wide_String) return Wide_String is

      Splited_Number : Wide_String (1 .. 2 * word'Last - 1);

   begin

      for I in word'First .. word'Last loop

         Splited_Number (2 * I - 1) := word (I);

         if I /= word'Last then
            Splited_Number (2 * I) := ' ';
         end if;
      end loop;

      return Splited_Number;

   end Split_Number;

   function Has_Comma (word : Wide_String) return Boolean is

      Comma : constant Wide_Character := ',';
   begin
      return word (word'Last) = Comma;
   end Has_Comma;

end Phonems_Utils;
