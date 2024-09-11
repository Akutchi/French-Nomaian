with Cairo;
with Glib; use Glib;
with Ada.Numerics;

package Draw_Glyphs is

   PI     : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   TWO_PI : constant Gdouble := 2.0 * PI;
   PI_2   : constant Gdouble := PI / 2.0;
   PI_3   : constant Gdouble := PI / 3.0;
   PI_4   : constant Gdouble := PI / 4.0;
   PI_6   : constant Gdouble := PI / 6.0;

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

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure SquareSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure PentaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HexaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

   procedure HeptaHepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble);

end Draw_Glyphs;
