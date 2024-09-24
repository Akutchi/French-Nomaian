with Ada.Numerics.Generic_Elementary_Functions;

with Cairo;
with Cairo.Surface;
with Cairo.SVG;

with Glib; use Glib;

with Locations;
with Draw_Glyphs;
with Draw_Unrolled_Spiral;
with Draw_Spiral;

with Math_Constants; use Math_Constants;

package body Tools is

   package C_S renames Cairo.Surface;
   package C_SVG renames Cairo.SVG;

   package DG renames Draw_Glyphs;
   package DUS renames Draw_Unrolled_Spiral;
   package DS renames Draw_Spiral;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   ---------------------
   -- To_Spiral_Model --
   ---------------------

   function To_Spiral_Model
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map) return P2G.Spiral_Model.Tree
   is

      Phonems_Version : constant S_WU.Unbounded_Wide_String :=
        S_WU.Null_Unbounded_Wide_String;

   begin

      declare
         Phonems : constant Wide_String :=
           P2G.Simplify (S2P.To_Phonems (Sentence, Phonems_Version, dict));

         Glyphs : P2G.List_GlyphInfo.Vector;
         Spiral : P2G.Spiral_Model.Tree := P2G.Spiral_Model.Empty_Tree;

      begin

         if Phonems'Length > 0 then
            Glyphs := P2G.To_Glyphs (Phonems, LM);
            P2G.Construct (Spiral, Glyphs);
         end if;

         return Spiral;
      end;

   end To_Spiral_Model;

   -----------------------
   -- Create_Linear_SVG --
   -----------------------

   procedure Create_Linear_SVG
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map)
   is

      Spiral : constant P2G.Spiral_Model.Tree :=
        Tools.To_Spiral_Model (Sentence, dict, LM);

   begin

      --  If no words are in the dictionnary (or we give an empty string),
      --  S2P return "" and so the tree is empty.
      if not P2G.Spiral_Model.Is_Empty (Spiral) then

         declare

            Root       : constant P2G.Spiral_Model.Cursor :=
              P2G.Spiral_Model.Root (Spiral);
            Root_Child : constant P2G.Spiral_Model.Cursor :=
              P2G.Spiral_Model.First_Child (Root);

            Depth_N : constant Positive :=
              Positive (P2G.Max_Depth (Root_Child, LM));

            W : constant Gdouble := Sqrt (100.0 * Gdouble (Depth_N));
            H : constant Gdouble := 30.0;

            SVG_Surface : constant Cairo.Cairo_Surface :=
              C_SVG.Create (Locations.SVG_FILE, W, H);

            Ctx : Cairo.Cairo_Context := Cairo.Create (SVG_Surface);

            state : DUS.Machine_State;

         begin

            DG.Background (Ctx, W, H);
            DUS.Draw_Unrolled_Spiral (Ctx, Root_Child, 5.0, H / 2.0, state);

            C_S.Finish (SVG_Surface);
            Cairo.Destroy (Ctx);
            C_S.Destroy (SVG_Surface);

         end;
      end if;

   end Create_Linear_SVG;

   -----------------------
   -- Create_Spiral_SVG --
   -----------------------

   procedure Create_Spiral_SVG
     (Sentence : S_WU.Unbounded_Wide_String; dict : S2P.Cmudict.Map;
      LM       : P2G.Language_Model.Map)
   is

      Spiral : constant P2G.Spiral_Model.Tree :=
        Tools.To_Spiral_Model (Sentence, dict, LM);

   begin

      if not P2G.Spiral_Model.Is_Empty (Spiral) then

         declare

            Root       : constant P2G.Spiral_Model.Cursor :=
              P2G.Spiral_Model.Root (Spiral);
            Root_Child : constant P2G.Spiral_Model.Cursor :=
              P2G.Spiral_Model.First_Child (Root);

            Depth_N : constant Natural :=
              Natural (P2G.Max_Depth (Root_Child, LM));

            R : constant Gdouble := Phi**2.0;
            W : constant Gdouble := R + R / Phi + Gdouble (Depth_N) * Phi;
            H : constant Gdouble := R + Gdouble (Depth_N) * Phi;

            SVG_Surface : constant Cairo.Cairo_Surface :=
              C_SVG.Create (Locations.SVG_FILE, W, H);

            Ctx : Cairo.Cairo_Context := Cairo.Create (SVG_Surface);

            state : DS.Machine_State;

         begin

            state.Xb      := W / 2.0;
            state.Yb      := H / 2.0;
            state.LM      := LM;
            state.Depth_N := Gdouble (Depth_N);

            DG.Background (Ctx, W, H);
            DS.Draw_Spiral (Ctx, Root_Child, state);

            C_S.Finish (SVG_Surface);
            Cairo.Destroy (Ctx);
            C_S.Destroy (SVG_Surface);

         end;
      end if;

   end Create_Spiral_SVG;

end Tools;
