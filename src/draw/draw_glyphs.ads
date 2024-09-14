with Cairo;
with Glib; use Glib;

with Draw_Utils; use Draw_Utils;

package Draw_Glyphs is

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble);

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble);

   procedure Dot (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble);

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Word_Separator (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive;
      r   :        Gdouble := R_Poly) with
     Pre => N > 3;

   procedure NgoneLine
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive) with
     Pre => N > 3;

   procedure NgoneBend
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive) with
     Pre => N > 3;

   procedure Draw_Ngone
     (Ctx : in out Cairo.Cairo_Context; GlyphName : String; X, Y : Gdouble;
      Has_Line, Has_Bend :        Boolean);

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Square (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Penta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Hexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure x2_Hepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

end Draw_Glyphs;
