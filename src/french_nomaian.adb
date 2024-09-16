with Ada.Strings.Wide_Unbounded;
with Ada.Numerics.Generic_Elementary_Functions;

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

with Draw_Unrolled_Spiral;

with Draw_Utils; use Draw_Utils;

procedure French_Nomaian is

   package S_WU renames Ada.Strings.Wide_Unbounded;
   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;
   package S2P renames Sentence2Phonems;
   package P2G renames Phonems2Glyphs;
   package DG renames Draw_Glyphs;
   package DS renames Draw_Spiral;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

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

      Depth_N : constant Positive :=
        24; --  Gdouble (P2G.Depth (Root_Child, LM));

      Start_Angle : constant Gdouble := TWO_PI;
      R           : constant Gdouble := Phi**(2.0 * Start_Angle / PI);

      W : constant Gdouble := R + R / Phi + 60.0;
      H : constant Gdouble := R + 60.0;

      SVG_Surface : constant Cairo.Cairo_Surface :=
        C_SVG.Create (Locations.SVG_FILE, W, H);

      Ctx : Cairo.Cairo_Context := Cairo.Create (SVG_Surface);

      state : DS.Machine_State;

   begin

      state.theta     := TWO_PI;
      state.Increment := TWO_PI / 50.0;
      state.LM        := LM;
      state.Depth_N   := Gdouble (Depth_N);

      DG.Background (Ctx, W, H);
      --  DS.Draw_Spiral (Ctx, Root_Child, state);
      DG.Rotation_Around (Ctx, W / 2.0, W / 2.0, PI_2);

      Draw_Unrolled_Spiral.Draw_Fibionnaci_Spiral
        (Ctx, W / 2.0, W / 2.0, Start_Angle, Depth_N);

      C_S.Finish (SVG_Surface);
      Cairo.Destroy (Ctx);
      C_S.Destroy (SVG_Surface);

   end;

end French_Nomaian;
