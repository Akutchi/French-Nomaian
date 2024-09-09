with Ada.Characters.Conversions;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;

with Ada.Wide_Text_IO;

package body Phonems_Utils is

   package CC renames Ada.Characters.Conversions;
   package S_WF renames Ada.Strings.Wide_Fixed;
   package S_WM renames Ada.Strings.Wide_Maps;

   package W_IO renames Ada.Wide_Text_IO;

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

         when '§' =>
            return 'ç';

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
            Splited_Number (2 * I) := Space_Char;
         end if;
      end loop;

      return Splited_Number;

   end Split_Number;

   ----------------
   -- Split_Word --
   ----------------

   function Split_Apostrophe_Word
     (word : Wide_String) return S_WU.Unbounded_Wide_String
   is

      Apostrophe_In_Word : constant Natural :=
        S_WF.Count (word, S_WM.To_Set (Apostrophe_Char));
      Splited_Word       : Wide_String (1 .. word'Last + Apostrophe_In_Word);
      Apostrophe_Count   : Natural          := 0;
   begin

      for I in word'First .. word'Last loop

         if word (I) = Apostrophe_Char then
            Splited_Word (I)     := Apostrophe_Char;
            Splited_Word (I + 1) := Space_Char;
            Apostrophe_Count     := Apostrophe_Count + 1;

         else
            Splited_Word (I + Apostrophe_Count) := word (I);

         end if;
      end loop;

      return S_WU.To_Unbounded_Wide_String (Splited_Word);

   end Split_Apostrophe_Word;

   ---------
   -- Has --
   ---------

   function Has
     (Char_Pattern : Wide_Character; word : Wide_String) return Boolean
   is
   begin
      return S_WF.Count (word, S_WM.To_Set (Char_Pattern)) /= 0;
   end Has;

   ---------------------
   -- Has_Final_Point --
   ---------------------

   function Has_Final_Point (word : Wide_String) return Boolean is

      Point : constant Boolean :=
        S_WF.Count (word, S_WM.To_Set (Point_Char)) = 1;

   begin
      return Point and then not Is_Float (word);
   end Has_Final_Point;

   ------------------
   -- Has_Ellipsis --
   ------------------

   function Has_Ellipsis (word : Wide_String) return Boolean is
   begin

      if word'Last < 3 then
         return False;
      end if;

      return
        word (word'Last - 2) = Point_Char
        and then word (word'Last - 1) = Point_Char
        and then word (word'Last) = Point_Char;
   end Has_Ellipsis;

   --------------------
   -- Split_Ellipses --
   --------------------

   function Split_Ellipses (word : Wide_String) return Wide_String is

      Splited_Word : Wide_String (1 .. word'Last + 6);
      I            : Natural := word'Last;
   begin

      Splited_Word (word'First .. word'Last - 3) :=
        word (word'First .. word'Last - 3);

      loop

         exit when I >= word'Last + 6;

         Splited_Word (I)     := Space_Char;
         Splited_Word (I + 1) := Point_Char;
         I                    := I + 2;
      end loop;

      Splited_Word (word'Last - 2) := Space_Char;
      Splited_Word (word'Last - 1) := Word_Separator;
      Splited_Word (word'Last)     := Space_Char;

      return Splited_Word;

   end Split_Ellipses;

end Phonems_Utils;
