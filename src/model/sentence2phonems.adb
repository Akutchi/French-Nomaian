with Ada.Command_Line;
with Ada.Characters.Conversions;
with Ada.Wide_Text_IO;
with Ada.Strings;
with Ada.Strings.Wide_Maps;
with Ada.Characters.Handling;

with Phonems_Utils; use Phonems_Utils;

package body Sentence2Phonems is

   package CLI renames Ada.Command_Line;
   package CC renames Ada.Characters.Conversions;
   package W_IO renames Ada.Wide_Text_IO;
   package Str renames Ada.Strings;
   package S_WM renames Ada.Strings.Wide_Maps;
   package CH renames Ada.Characters.Handling;

   -----------------------
   -- Get_Raw_Sentences --
   -----------------------

   function Get_Raw_Sentence return S_WU.Unbounded_Wide_String is

      Sentence : S_WU.Unbounded_Wide_String := S_WU.Null_Unbounded_Wide_String;

   begin

      for I in 1 .. CLI.Argument_Count loop

         S_WU.Append (Sentence, CC.To_Wide_String ((CLI.Argument (I))));
         S_WU.Append (Sentence, " ");

      end loop;

      return Sentence;

   end Get_Raw_Sentence;

   ---------------
   -- To_French --
   ---------------

   procedure To_French (Sentence : in out S_WU.Unbounded_Wide_String) is

      Latin_Base    : constant Wide_Character := 'Ã';
      Accented_Char : Wide_Character;
      To_Remove     : Latin_Base_Companion_Char_Index.Vector;
      de            : Natural                 := 0;
   begin

      if S_WU.Length (Sentence) /= 0 then

         for I in 1 .. S_WU.Length (Sentence) - 1 loop

            if S_WU.Element (Sentence, I) = Latin_Base then

               Accented_Char :=
                 Get_Accented_Character (S_WU.Element (Sentence, I + 1));
               S_WU.Replace_Element (Sentence, I, Accented_Char);
               Latin_Base_Companion_Char_Index.Append (To_Remove, I + 1);

            end if;
         end loop;

         if not Latin_Base_Companion_Char_Index.Is_Empty (To_Remove) then
            for E of To_Remove loop
               S_WU.Delete (Sentence, E - de, Natural (E - de));
               de := de + 1;
            end loop;
         end if;

      end if;

   end To_French;

   ---------------------------
   -- Get_Word_And_Phonetic --
   ---------------------------

   procedure Get_Word_And_Phonetic
     (File_Line : Wide_String; Dict_Tuple : out Wide_Tuple)
   is

      k : Natural := 0;

      Index : Natural := 1;
      F     : Positive;
      L     : Natural;

      Plus : constant S_WM.Wide_Character_Set := S_WM.To_Set ('+');

   begin

      while Index in File_Line'Range loop

         S_WU.Find_Token
           (Source => S_WU.To_Unbounded_Wide_String (File_Line), Set => Plus,
            From   => Index, Test => Str.Outside, First => F, Last => L);

         exit when L = 0;

         Dict_Tuple (k) := S_WU.To_Unbounded_Wide_String (File_Line (F .. L));

         Index := L + 1;
         k     := k + 1;

      end loop;

   end Get_Word_And_Phonetic;

   -----------------------------
   -- Is_Not_Phonetic_Variant --
   -----------------------------

   function Is_Not_Phonetic_Variant
     (Word : S_WU.Unbounded_Wide_String) return Boolean
   is
      Closing_Parenthesis : constant Wide_Character := ')';
      Word_Bounded        : constant Wide_String := S_WU.To_Wide_String (Word);

   begin
      return Word_Bounded (Word_Bounded'Last) /= Closing_Parenthesis;

   end Is_Not_Phonetic_Variant;

   ------------------
   -- Init_Cmudict --
   ------------------

   procedure Init_Cmudict (dict : in out Cmudict.Map) is

      File : W_IO.File_Type;

      Comment : constant Wide_Character := ';';

   begin

      W_IO.Open (File, W_IO.In_File, Cmudict_Location);

      while not W_IO.End_Of_File (File) loop

         declare
            File_Line  : constant Wide_String := W_IO.Get_Line (File);
            Dict_Tuple : Wide_Tuple;
         begin

            if File_Line (File_Line'First) /= Comment then

               Get_Word_And_Phonetic (File_Line, Dict_Tuple);

               if Is_Not_Phonetic_Variant (Dict_Tuple (0)) then

                  dict.Include
                    (S_WU.To_Wide_String (Dict_Tuple (0)),
                     S_WU.To_Wide_String (Dict_Tuple (1)));
               end if;
            end if;
         end;
      end loop;

      W_IO.Close (File);

   end Init_Cmudict;

   procedure Decide_On_Word
     (Phonems : in out S_WU.Unbounded_Wide_String; dict : Cmudict.Map;
      word    :        Wide_String)
   is

      Slice_End : Positive         := word'Last;
      Comma     : constant Boolean := Has_Comma (word);

   begin

      if Comma then
         Slice_End := Slice_End - 1;
      end if;

      declare
         Sliced_Word : constant Wide_String := word (word'First .. Slice_End);
      begin

         if dict.Contains (Sliced_Word) then
            S_WU.Append (Phonems, dict (Sliced_Word) & "|");

         elsif Is_Integer (Sliced_Word) then
            S_WU.Append (Phonems, Split_Number (Sliced_Word) & "|");

         elsif Is_Float (Sliced_Word) then
            S_WU.Append (Phonems, Split_Number (Sliced_Word) & "|");

         else
            W_IO.Put_Line
              ("Warning : '" & word &
               "' is not in the dictionnary. Consider adding it.");
         end if;

         if Comma then
            S_WU.Append (Phonems, ",|");
         end if;
      end;

   end Decide_On_Word;

   ----------------
   -- To_Phonems --
   ----------------

   function To_Phonems
     (Sentence : S_WU.Unbounded_Wide_String) return Wide_String
   is

      dict : Cmudict.Map;

      Phonems_Version : S_WU.Unbounded_Wide_String;

      Index : Natural := 1;
      F     : Positive;
      L     : Natural;

      Whitespace : constant S_WM.Wide_Character_Set := S_WM.To_Set (' ');

   begin

      Init_Cmudict (dict);

      while Index in 1 .. S_WU.Length (Sentence) loop

         S_WU.Find_Token
           (Source => Sentence, Set => Whitespace, From => Index,
            Test   => Str.Outside, First => F, Last => L);

         exit when L = 0;

         declare
            word : constant Wide_String :=
              CC.To_Wide_String
                (CH.To_Lower (CC.To_String (S_WU.Slice (Sentence, F, L))));
         begin
            Decide_On_Word (Phonems_Version, dict, word);
         end;

         Index := L + 1;

      end loop;

      return S_WU.To_Wide_String (Phonems_Version);

   end To_Phonems;

end Sentence2Phonems;
