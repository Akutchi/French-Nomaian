with Ada.Strings.Wide_Unbounded;

with Cairo;
with Cairo.Surface;
with Cairo.SVG;

with Locations;
with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;
with Draw_Spiral;
with Draw_Utils;

with Glib; use Glib;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;
   package DS renames Draw_Spiral;
   package DU renames Draw_Utils;

   Sentence   : S_WU.Unbounded_Wide_String;
   Spiral     : P2G.Spiral_Model.Tree;
   Root_Child : P2G.Spiral_Model.Cursor;

   dict : S2P.Cmudict.Map;
   LM   : P2G.Language_Model.Map;

begin

   S2P.Init_Cmudict (dict);
   P2G.Init_Language_Model (LM);

   Sentence   := S2P.Get_Raw_Sentence;
   Spiral     := Tools.To_Spiral_Model (Sentence, dict, LM);
   Root_Child := P2G.Spiral_Model.First_Child (P2G.Spiral_Model.Root (Spiral));

   declare

      Tree_Length : constant Float := P2G.Depth (Root_Child, LM);

      SVG_Surface : constant Cairo.Cairo_Surface :=
        C_SVG.Create (Locations.SVG_FILE, 150.0, 150.0);

      Ctx : Cairo.Cairo_Context := Cairo.Create (SVG_Surface);

      state : DS.Machine_State;

   begin

      DS.Background (Ctx);
      DS.Draw_Unrolled_Spiral (Ctx, Root_Child, 50.0, 50.0, state);
      --  DS.Draw_Base_Spiral (Ctx, 50.0, 50.0);

      C_S.Finish (SVG_Surface);
      Cairo.Destroy (Ctx);
      C_S.Destroy (SVG_Surface);

   end;

end French_Nomaian;
