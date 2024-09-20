with Cairo;
with Glib; use Glib;

with Phonems2Glyphs;

with Draw_Constants; use Draw_Constants;

package Draw_Glyphs is

   package P2G renames Phonems2Glyphs;

   procedure Background (Ctx : in out Cairo.Cairo_Context; W, H : Gdouble);

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble);

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble);

   procedure Dot
     (Ctx  : in out Cairo.Cairo_Context; X, Y : Gdouble;
      Rdot :        Gdouble := R_Dot);

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

   procedure Choose_Glyph
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; GN_String : String);

end Draw_Glyphs;
