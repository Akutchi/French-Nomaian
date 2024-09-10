with Cairo;
with Glib; use Glib;

package Draw_Glyphs is

   R_Dot  : constant Gdouble := 0.3;
   R_Poly : constant Gdouble := 5.0;
   PI_4   : constant Gdouble := -0.785;

   dx : Gdouble := 5.0;
   dy : Gdouble := 0.0;

   procedure Background (Ctx : Cairo.Cairo_Context);

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive) with
     Pre => N > 3;

end Draw_Glyphs;
