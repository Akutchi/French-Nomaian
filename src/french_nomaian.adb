with Ada.Wide_Text_IO.Wide_Unbounded_IO;
use Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with Sentence2Phonems; use Sentence2Phonems;
procedure French_Nomaian is

   Sentence : Unbounded_Wide_String;
begin

   Sentence := Get_Raw_Sentence;
   To_French (Sentence);
   Put_Line (Sentence);

end French_Nomaian;
