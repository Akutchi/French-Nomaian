with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;           use Ada.Wide_Text_IO;

with Sentence2Phonems; use Sentence2Phonems;
with Phonems2Glyphs;   use Phonems2Glyphs;
procedure French_Nomaian is

   Sentence        : Unbounded_Wide_String;
   Phonems_Version : constant Unbounded_Wide_String :=
     S_WU.Null_Unbounded_Wide_String;

   dict : Cmudict.Map;
   LM   : Language_Model.Map;

begin

   Init_Cmudict (dict);
   Init_Language_Model (LM);

   Sentence := Get_Raw_Sentence;

   declare
      Phonems : constant Wide_String :=
        Simplify (To_Phonems (Sentence, Phonems_Version, dict));

      Glyphs : constant List_GlyphInfo.Vector := To_Glyphs (Phonems, LM);

   begin

      Put_Line (Phonems);
      print_glyphs (Glyphs);

   end;

end French_Nomaian;
