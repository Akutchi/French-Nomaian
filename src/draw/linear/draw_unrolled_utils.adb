with Draw_Glyphs;
with Draw_Utils;

with Math_Constants; use Math_Constants;

package body Draw_Unrolled_Utils is

   package DG renames Draw_Glyphs;
   package DU renames Draw_Utils;

   ------------------
   -- Is_Start_Dot --
   ------------------

   function Is_Start_Dot (E : P2G.GlyphInfo) return Boolean is
   begin
      return E.T = P2G.Starting_Dot;
   end Is_Start_Dot;

   ---------------------------
   -- dx_For_Word_Separator --
   ---------------------------

   function dx_For_Word_Separator
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble
   is

      GN_String : constant String := S_U.To_String (GlyphName);
   begin

      case GlyphRep'Value (GN_String) is

         when line =>
            case dp is
               when after =>
                  return 0.0;
               when before =>
                  return Line_Glyph_R_Poly;
            end case;

         when bend =>

            case dp is
               when before =>
                  return 2.0 * R_Poly;
               when after =>
                  return -0.5 * R_Poly;
            end case;

         when linedotline =>
            return 2.0 * R_Poly;

         when square | hexa | octa =>
            return R_Poly;

         when squareline | squarebend =>
            return 0.7 * R_Poly;

         when squaresquare =>

            case dp is
               when before =>
                  return 2.0 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when penta =>

            case dp is
               when before =>
                  return 1.0 * R_Poly;
               when after =>
                  return 0.83 * R_Poly;
            end case;

         when pentaline | pentabend =>

            case dp is
               when before =>
                  return 0.8 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when pentasquare =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 0.8 * R_Poly;
            end case;

         when pentapenta =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 2.6 * R_Poly;
            end case;

         when hexaline | hexabend | heptaline | heptabend =>

            case dp is
               when before =>
                  return 0.9 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when hexasquare =>
            return R_Poly;

         when hexapenta =>

            case dp is
               when before =>
                  return 1.95 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when hexahexa =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 2.5 * R_Poly;
            end case;

         when hepta =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 0.9 * R_Poly;
            end case;

         when heptahexa =>
            return 0.0;

         when heptahepta =>
            return 0.0;

         when octaline =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when others =>
            return 0.0;

      end case;

   end dx_For_Word_Separator;

   ---------------------------
   -- dy_For_Word_Separator --
   ---------------------------

   function dy_For_Word_Separator
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble
   is

      GN_String : constant String := S_U.To_String (GlyphName);
   begin

      case GlyphRep'Value (GN_String) is

         when bend =>
            case dp is
               when before =>
                  return 1.4 * R_Poly;
               when after =>
                  return 0.0;
            end case;

         when line | square =>
            case dp is
               when before =>
                  return 0.4 * R_Poly;
               when after =>
                  return 0.0;
            end case;

         when squareline | squarebend =>

            case dp is
               when before =>
                  return -0.3 * R_Poly;
               when after =>
                  return 0.73 * R_Poly;
            end case;

         when squaresquare =>

            case dp is
               when before =>
                  return 0.4 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when penta =>

            case dp is
               when before =>
                  return -0.2 * R_Poly;
               when after =>
                  return 0.0;
            end case;

         when pentaline | pentabend =>

            case dp is
               when before =>
                  return 0.4 * R_Poly;
               when after =>
                  return 0.3;
            end case;

         when pentasquare =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when pentapenta =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when others =>
            return 0.0;

      end case;

   end dy_For_Word_Separator;

   -------------------
   -- Branch_Offset --
   -------------------

   function Branch_Offset (Element : P2G.GlyphInfo) return Gdouble is

      de_base : constant Gdouble := 1.5 * R_Poly;
   begin

      case GlyphRep'Value (S_U.To_String (Element.GlyphName)) is

         when pentapenta | hexasquare | hexapenta | heptasquare | heptapenta =>
            return de_base + R_Poly;

         when others =>
            return de_base;
      end case;

   end Branch_Offset;

   ---------------------------------------
   -- Get_Element_Displacement_For_Line --
   ---------------------------------------

   procedure Get_Element_Displacement_For_Line
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; dp : dpos_Type)
   is
   begin

      dx_e := 0.0;
      dy_e := 0.0;

      case GlyphRep'Value (S_U.To_String (Element.GlyphName)) is

         when line =>

            if dp = after then
               dx_e := Line_Glyph_R_Poly;
            end if;

         when bend =>

            if dp = before then
               dx_e := R_Poly_2;
               dy_e := -R_Poly;
            else
               dx_e := 2.0 * R_Poly;
               dy_e := 0.0;
            end if;

         when square | hexa | octa =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := -dx_e;
            end if;

         when squareline | squarebend =>

            dx_e := R_Poly * Cos (PI_4);
            dy_e := R_Poly * Sin (PI_4);

            if dp = before then
               dx_e := -dx_e;
            end if;

         when squaresquare =>

            if dp = before then
               dx_e := -R_Poly;

            elsif dp = after then
               dx_e := dx_e + 2.0 * R_Poly;
               dy_e := R_Poly;

            end if;

         when penta | pentasquare =>

            dx_e := R_Poly;

            if dp = before then
               dx_e := R_Poly * Cos (4.0 * PI_5);
               dy_e := R_Poly * Sin (4.0 * PI_5);
            end if;

         when pentaline | pentabend =>

            if dp = before then
               dx_e := -R_Poly;
            else
               dx_e := R_Poly * Cos (PI_5);
               dy_e := R_Poly * Sin (PI_5);
            end if;

         when pentapenta =>

            if dp = before then
               dx_e := -2.5 * R_Poly;
            else
               dx_e := R_Poly;
            end if;

         when hexaline | hexabend =>

            dx_e := R_Poly * Cos (5.0 * PI_6);
            dy_e := R_Poly * Sin (5.0 * PI_6);

            if dp = after then
               dx_e := -dx_e;
            end if;

         when hexasquare =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := R_Poly * (1.0 + Cos (PI_6));
               dy_e := -R_Poly * Sin (PI_6);
            end if;

         when hexapenta =>

            dx_e := -R_Poly;
            if dp = after then
               dx_e := R_Poly * (1.0 + Cos (PI_15));
               dy_e := -R_Poly * Sin (PI_15);
            end if;

         when hexahexa =>

            if dp = before then
               dx_e := -R_Poly * (2.0 + Cos (-PI_3));
               dy_e := -R_Poly * Sin (PI_3);
            end if;

         when hepta =>

            dx_e := R_Poly;
            if dp = before then
               dx_e := R_Poly * Cos (6.0 * PI_7);
               dy_e := R_Poly * Sin (6.0 * PI_7);
            end if;

         when heptaline | heptabend =>

            if dp = before then
               dx_e := -R_Poly;
            else
               dx_e := R_Poly * Cos (5.0 * PI_7);
               dy_e := R_Poly * Sin (5.0 * PI_7);

            end if;

         when octaline =>

            dx_e := R_Poly * Cos (5.0 * PI_6);
            dy_e := R_Poly * Sin (5.0 * PI_6);

            if dp = after then
               dx_e := -dx_e;
            end if;

         when others =>
            dx_e := 0.0;
            dy_e := 0.0;

      end case;

   end Get_Element_Displacement_For_Line;

   ------------------------
   -- Line_Between_Words --
   ------------------------

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; Parent, Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp :        Gdouble)
   is

      Xp_t : Gdouble := Xp;
      Yp_t : Gdouble := Yp;
      Xc_t : Gdouble := Xc;
      Yc_t : Gdouble := Yc;

      dx_Parent, dx_Child : Gdouble := 0.0;
      dy_Parent, dy_Child : Gdouble := 0.0;

   begin

      Get_Element_Displacement_For_Line (Parent, dx_Parent, dy_Parent, after);
      Get_Element_Displacement_For_Line (Child, dx_Child, dy_Child, before);

      Xp_t := Xp_t + dx_Parent;
      Yp_t := Yp_t + dy_Parent;

      Xc_t := Xc_t + dx_Child;
      Yc_t := Yc_t + dy_Child;

      DG.Dot (Ctx, Xp_t, Yp_t);

      Cairo.Move_To (Ctx, Xp_t, Yp_t);
      Cairo.Line_To (Ctx, Xc_t, Yc_t);
      Cairo.Stroke (Ctx);

      DG.Dot (Ctx, Xc_t, Yc_t);

   end Line_Between_Words;

   ---------------------------------
   -- Get_Displacement_For_Branch --
   ---------------------------------

   procedure Get_Displacement_For_Branch
     (Element              : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble;
      Is_Vowel, Is_Numeral : Boolean)
   is

      r     : Gdouble := R_Poly;
      theta : Gdouble := 0.0; --  turn in anti-trigonometric sense.
   begin

      case GlyphRep'Value (S_U.To_String (Element.GlyphName)) is

         when bend =>

            if not Is_Vowel then
               r     := 1.2 * R_Poly;
               theta := 2.0;
            end if;

         when linedotline =>
            r     := -R_Poly;
            theta := -0.23;

         when square =>
            theta := PI_2;

         when squareline | squarebend =>

            r     := -R_Poly;
            theta := 3.0 * PI_4;

            if not Is_Vowel then
               theta := PI + 0.7;
            end if;

         when squaresquare =>
            r     := 2.0 * R_Poly;
            theta := PI_2;

         when penta =>
            theta := PI_4 + 0.1;

         when pentaline | pentabend =>

            if Is_Vowel then
               theta := -(PI_4 + 0.5);
            else
               r     := 1.4 * R_Poly;
               theta := PI_2 - 0.3;
            end if;

         when pentasquare =>

            theta := 0.5;

            if Is_Vowel then
               theta := -0.5;
            end if;

         when pentapenta =>

            if Is_Vowel then
               r     := -2.15 * R_Poly;
               theta := 3.0 * PI_4 + 0.4;
            else
               theta := PI_2 + 0.3;
            end if;

         when hexa =>
            theta := PI_4 + 0.2;

         when hexaline | hexabend =>

            if Is_Vowel then
               theta := -0.4;

            elsif Is_Numeral then
               theta := 0.4;

            else
               theta := PI_3 - 0.5;
            end if;

         when hexasquare | hexapenta =>

            if not Is_Vowel then
               theta := PI_3 - 0.2;
            end if;

         when hexahexa =>

            if Is_Vowel then
               theta := -PI_6 - 0.5;
            else
               theta := PI_2 + 0.5;
            end if;

         when hepta =>
            theta := 0.2;

         when heptaline | heptabend =>

            if Is_Vowel then
               theta := -PI_7 - 0.4;
            else
               theta := PI_2;
            end if;

         when heptasquare | heptapenta =>
            theta := -PI_7;

         when octa =>
            theta := PI_2 - 0.8;

         when octaline =>
            theta := -0.2;

         when others =>
            r     := 0.0;
            theta := 0.0;

      end case;

      dx_e := -r * Cos (theta);
      dy_e := -r * Sin (theta);

   end Get_Displacement_For_Branch;

   -----------------
   -- Draw_Branch --
   -----------------

   procedure Draw_Branch
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble)
   is

      Xp_t : Gdouble := Xp;
      Yp_t : Gdouble := Yp;
      Xc_t : Gdouble := Xc;
      Yc_t : Gdouble := Yc;

      dx_root, dy_root   : Gdouble := 0.0;
      dx_child, dy_child : Gdouble := 0.0;

      Is_Vowel   : constant Boolean := Child.T = P2G.Vowel;
      Is_Numeral : constant Boolean := Child.T = P2G.Numeral;

   begin

      if DU.Is_CS_V (Parent, Child) or else DU.Is_DX (Parent, Child, P2G.Vowel)
        or else DU.Is_CS_N (Parent, Child)
        or else DU.Is_DX (Parent, Child, P2G.Numeral)
      then

         Get_Displacement_For_Branch (Parent, dx_root, dy_root, False, False);
         Get_Displacement_For_Branch
           (Child, dx_child, dy_child, Is_Vowel, Is_Numeral);

         Xp_t := Xp_t + dx_root;
         Yp_t := Yp_t + dy_root;

         Xc_t := Xc_t + dx_child;
         Yc_t := Yc_t + dy_child;

         Cairo.Move_To (Ctx, Xp_t, Yp_t);
         Cairo.Line_To (Ctx, Xc_t, Yc_t);
         Cairo.Stroke (Ctx);

      end if;

   end Draw_Branch;

end Draw_Unrolled_Utils;
