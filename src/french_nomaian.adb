with Ada.Strings.Wide_Unbounded;

with Cairo;
with Cairo.Surface;
with Cairo.SVG;

with Glib; use Glib;

with Locations;
with Sentence2Phonems;
with Phonems2Glyphs;
with Tools;
with Draw_Glyphs;
with Draw_Spiral;

with Draw_Utils; use Draw_Utils;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;
   package DG renames Draw_Glyphs;
   package DS renames Draw_Spiral;

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

      W : constant Gdouble := 200.0;
      H : constant Gdouble := 200.0;

      SVG_Surface : constant Cairo.Cairo_Surface :=
        C_SVG.Create (Locations.SVG_FILE, W, H);

      Ctx : Cairo.Cairo_Context := Cairo.Create (SVG_Surface);

      state : DS.Machine_State;

   begin

      state.theta     := TWO_PI;
      state.Increment := TWO_PI / Gdouble (P2G.Depth (Root_Child, LM));

      DG.Background (Ctx, W, H);
      DS.Draw_Spiral (Ctx, Root_Child, state);

      C_S.Finish (SVG_Surface);
      Cairo.Destroy (Ctx);
      C_S.Destroy (SVG_Surface);

   end;

end French_Nomaian;
