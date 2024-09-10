with Cairo;
with Glib; use Glib;

package Draw_Glyphs is

   R_Dot  : constant Gdouble := 0.3;
   R_Poly : constant Gdouble := 5.0;

   procedure Background (Ctx : Cairo.Cairo_Context);

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble);

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble);

   procedure Dot (Ctx : Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

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

   procedure SquareSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure PentaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaHepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

end Draw_Glyphs;
