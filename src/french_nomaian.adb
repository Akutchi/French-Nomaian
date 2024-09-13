with Ada.Strings.Wide_Unbounded;

with Cairo;
with Cairo.Surface;
with Cairo.SVG;

with Locations;
with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;
with Draw_Glyphs;

with Glib; use Glib;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;
   package DG renames Draw_Glyphs;

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
   Root_Child :=
     P2G.Spiral_Model.First_Child
       (P2G.Spiral_Model.First_Child (P2G.Spiral_Model.Root (Spiral)));

   declare

      L : constant Float := P2G.Depth (Root_Child, LM);

      S : constant Cairo.Cairo_Surface :=
        C_SVG.Create (Locations.SVG_FILE, 5.0 * Gdouble (L), 13.0);

      Ctx : Cairo.Cairo_Context := Cairo.Create (S);

      state : DG.Machine_State;

   begin

      P2G.Print (Root_Child);

      DG.Background (Ctx);
      DG.Draw_Unrolled_Spiral (Ctx, 5.0, 6.0, state, Root_Child);
      C_S.Finish (S);
      Cairo.Destroy (Ctx);
      C_S.Destroy (S);

   end;

end French_Nomaian;
