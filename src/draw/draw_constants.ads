with Glib; use Glib;

package Draw_Constants is

   type GlyphRep is
     (dot_start, line, bend, square, penta, hexa, hepta, octa, squareline,
      pentaline, hexaline, heptaline, octaline, squarebend, pentabend,
      hexabend, heptabend, octabend, squaresquare, pentapenta, hexahexa,
      heptahepta, pentasquare, hexasquare, hexapenta, heptasquare, heptapenta,
      heptahexa, linedotline);

   type dpos_Type is (before, after);

   R_Poly     : constant Gdouble := 0.5;
   Line_Width : constant Gdouble := 0.1 * R_Poly;
   R_Dot      : constant Gdouble := 0.2 * R_Poly;

   R_Poly_2          : constant Gdouble := R_Poly / 2.0;
   Line_Glyph_R_Poly : constant Gdouble := 1.3 * R_Poly;
   Line_Words_R_Poly : constant Gdouble := 2.0 * R_Poly;

   Offset_Leaf   : constant Gdouble := 0.5 * R_Poly;
   Offset_Branch : constant Gdouble := 1.5 * R_Poly;

   dy_vn : constant Gdouble := 3.0;

end Draw_Constants;
