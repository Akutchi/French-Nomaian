with Cairo;

with Glib; use Glib;

with Phonems2Glyphs;

with Draw_Constants; use Draw_Constants;

package Draw_Spiral_Utils is

   package P2G renames Phonems2Glyphs;

   type LineInfo is record

      Parent, Child : P2G.GlyphInfo;
      Xp, Yp        : Gdouble := 0.0;
      Xc, Yc        : Gdouble := 0.0;

   end record;

   procedure Draw_Vector_Field
     (Ctx : Cairo.Cairo_Context; X, Y, I, N, radius_var, theta_var : Gdouble);

   procedure Get_Element_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type);
   --  Turn in the anti-trigonometric sense.
   --  Some values were not implemented because not necessary at the time of
   --  writing (17/09/24)

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble);

   procedure Draw_With_Coordinates
     (I, N :        Gdouble; Line_Info : in out LineInfo;
      Ctx  : in out Cairo.Cairo_Context);

end Draw_Spiral_Utils;
