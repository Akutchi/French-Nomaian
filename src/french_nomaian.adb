with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;           use Ada.Wide_Text_IO;

with Sentence2Phonems; use Sentence2Phonems;
procedure French_Nomaian is

   Sentence : Unbounded_Wide_String;
begin

   Sentence := Get_Raw_Sentence;
   To_French (Sentence);

   declare
      Phonems : constant Wide_String := To_Phonems (Sentence);
   begin
      Put_Line (Phonems);
   end;

end French_Nomaian;
