with Ada.Strings.Unbounded;
with Ada.Numerics;

with Glib; use Glib;

with Phonems2Glyphs;

package Draw_Utils is

   package S_U renames Ada.Strings.Unbounded;

   package P2G renames Phonems2Glyphs;

   type GlyphRep is
     (dot_start, line, bend, square, penta, hexa, hepta, octa, squareline,
      pentaline, hexaline, heptaline, octaline, squarebend, pentabend,
      hexabend, heptabend, octabend, squaresquare, pentapenta, hexahexa,
      heptahepta, pentasquare, hexasquare, hexapenta, heptasquare, heptapenta,
      heptahexa, linedotline);

   type dpos_Type is (before, after);

   PI     : constant Gdouble := Gdouble (Ada.Numerics.Pi);
   TWO_PI : constant Gdouble := 2.0 * PI;
   PI_2   : constant Gdouble := PI / 2.0;
   PI_3   : constant Gdouble := PI / 3.0;
   PI_4   : constant Gdouble := PI / 4.0;
   PI_6   : constant Gdouble := PI / 6.0;

   Line_Width : constant Gdouble := 0.3;
   R_Dot      : constant Gdouble := 0.3;

   R_Poly            : constant Gdouble := 1.0;
   R_Poly_2          : constant Gdouble := R_Poly / 2.0;
   Line_Glyph_R_Poly : constant Gdouble := 1.3 * R_Poly;
   Line_Words_R_Poly : constant Gdouble := 2.0 * R_Poly;

   Offset_Branch : constant Gdouble := R_Poly;

   dy : constant Gdouble := 3.5;

   function Is_CX
     (E : P2G.GlyphInfo; Child_Type, X : Character) return Boolean;

   function Is_SX
     (E : P2G.GlyphInfo; Child_Type, X : Character) return Boolean;

   function Is_CS_V
     (Parent : P2G.GlyphInfo; Element : P2G.GlyphInfo) return Boolean;

   function Is_CS_N
     (Parent : P2G.GlyphInfo; Element : P2G.GlyphInfo) return Boolean;

   function Is_Start_Dot (E : P2G.GlyphInfo) return Boolean;

   function dx
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble;

   function Need_Line_Between_Phonems
     (Root : P2G.Spiral_Model.Cursor; Root_GlyphName : String) return Boolean;

end Draw_Utils;
