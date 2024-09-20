with Ada.Strings.Wide_Unbounded;

with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;

   Sentence : S_WU.Unbounded_Wide_String;

   dict : S2P.Cmudict.Map;
   LM   : P2G.Language_Model.Map;

begin

   S2P.Init_Cmudict (dict);
   P2G.Init_Language_Model (LM);

   Sentence := S2P.Get_Raw_Sentence;
   Tools.Create_Spiral_SVG (Sentence, dict, LM);

end French_Nomaian;
