with Ada.Strings.Wide_Unbounded;

with Cairo;
with Cairo.Surface;
with Cairo.SVG;

with Locations;
with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;
with Draw_Glyphs;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;
   package DG renames Draw_Glyphs;

   Sentence : S_WU.Unbounded_Wide_String;
   Spiral   : P2G.Spiral_Model.Tree;

   dict : S2P.Cmudict.Map;
   LM   : P2G.Language_Model.Map;

   S   : constant Cairo.Cairo_Surface :=
     C_SVG.Create (Locations.SVG_FILE, 100.0, 100.0);
   Ctx : Cairo.Cairo_Context          := Cairo.Create (S);

begin

   S2P.Init_Cmudict (dict);
   P2G.Init_Language_Model (LM);

   Sentence := S2P.Get_Raw_Sentence;
   Spiral   := Tools.To_Spiral_Model (Sentence, dict, LM);

   DG.Background (Ctx);
   DG.Ngone (Ctx, 50.0, 50.0, 5);

   C_S.Finish (S);
   Cairo.Destroy (Ctx);
   C_S.Destroy (S);

end French_Nomaian;
