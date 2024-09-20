with Cairo;

with Glib; use Glib;

with Phonems2Glyphs;

with Draw_Constants; use Draw_Constants;

package Draw_Spiral_Utils is

   package P2G renames Phonems2Glyphs;

   procedure Get_Element_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type);
   --  Turn in the anti-trigonometric sense.
   --  Some values were not implemented because not necessary at the time of
   --  writing (17/09/24)

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble);

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean);

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble);
   --  Also turn in the anti-trigonometric sense.

end Draw_Spiral_Utils;
