with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers; use Ada.Containers;

package body Draw_Utils is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   -----------
   -- Is_CX --
   -----------

   function Is_CX (E : P2G.GlyphInfo; Child_Type, X : Character) return Boolean
   is
   begin
      return E.T = 'c' and then Child_Type = X;
   end Is_CX;

   -----------
   -- Is_SX --
   -----------

   function Is_SX (E : P2G.GlyphInfo; Child_Type, X : Character) return Boolean
   is
   begin
      return E.T = 's' and then Child_Type = X;
   end Is_SX;

   -------------
   -- Is_CS_V --
   -------------

   function Is_CS_V
     (Parent : P2G.GlyphInfo; Element : P2G.GlyphInfo) return Boolean
   is
   begin
      return
        Is_CX (Parent, Element.T, P2G.Vowel)
        or else Is_SX (Parent, Element.T, P2G.Vowel);
   end Is_CS_V;

   -------------
   -- Is_CS_N --
   -------------

   function Is_CS_N
     (Parent : P2G.GlyphInfo; Element : P2G.GlyphInfo) return Boolean
   is
   begin
      return
        Is_CX (Parent, Element.T, P2G.Numeral)
        or else Is_SX (Parent, Element.T, P2G.Numeral);
   end Is_CS_N;

   ------------------
   -- Is_Start_Dot --
   ------------------

   function Is_Start_Dot (E : P2G.GlyphInfo) return Boolean is
   begin
      return E.T = 'd';
   end Is_Start_Dot;

   --------
   -- dx --
   --------

   function dx
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble
   is

      GN_String : constant String := S_U.To_String (GlyphName);
   begin

      case GlyphRep'Value (GN_String) is

         when dot_start =>
            return 0.0;

         when line =>
            return Line_Glyph_R_Poly;

         when bend =>

            case dp is
               when before =>
                  return 2.0 * R_Poly;
               when after =>
                  return -0.5;
            end case;

         when linedotline =>
            return 2.0 * R_Poly;

         when square | hexa | octa =>
            return R_Poly;

         when penta =>

            case dp is
               when before =>
                  return 1.0 * R_Poly;
               when after =>
                  return 0.83 * R_Poly;
            end case;

         when squareline | squarebend =>
            return 0.7 * R_Poly;

         when pentaline | pentabend =>

            case dp is
               when before =>
                  return 0.8 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when hexaline | hexabend | heptaline | heptabend =>

            case dp is
               when before =>
                  return 0.9 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when octaline | octabend =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when squaresquare =>

            case dp is
               when before =>
                  return 2.0 * R_Poly;
               when after =>
                  return R_Poly;
            end case;

         when pentapenta =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 2.6 * R_Poly;
            end case;

         when hexahexa =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 2.5 * R_Poly;
            end case;

         when heptahepta =>

            return 0.0;

         when pentasquare =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 0.8 * R_Poly;
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

         when heptasquare =>
            return 0.0;

         when heptapenta =>
            return 0.0;

         when heptahexa =>
            return 0.0;

         when others =>
            return 0.0;

      end case;

   end dx;

   --------
   -- dy --
   --------

   function dy
     (GlyphName : S_U.Unbounded_String; dp : dpos_Type) return Gdouble
   is

      GN_String : constant String := S_U.To_String (GlyphName);
   begin

      case GlyphRep'Value (GN_String) is

         when bend =>

            case dp is
               when before =>
                  return R_Poly;
               when after =>
                  return 0.0;
            end case;

         when linedotline =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.4;
            end case;

         when octa =>
            return 0.0;

         when penta =>

            case dp is
               when before =>
                  return -1.1 * R_Poly_2;
               when after =>
                  return 0.0;
            end case;

         when squareline | squarebend =>

            case dp is
               when before =>
                  return -0.7 * R_Poly;
               when after =>
                  return 0.0;
            end case;

         when pentaline | pentabend =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.6;
            end case;

         when hexaline | hexabend | heptaline | heptabend =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when octaline | octabend =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when squaresquare =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return R_Poly;
            end case;

         when pentapenta =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when hexahexa =>

            case dp is
               when before =>
                  return 0.3;
               when after =>
                  return 0.0;
            end case;

         when heptahepta =>
            return 0.0;

         when pentasquare =>

            case dp is
               when before =>
                  return 0.0;
               when after =>
                  return 0.0;
            end case;

         when hexasquare =>
            return 0.0;

         when heptasquare =>
            return 0.0;

         when heptapenta =>
            return 0.0;

         when heptahexa =>
            return 0.0;

         when others =>
            return 0.0;

      end case;

   end dy;
   -------------------------------
   -- Need_Line_Between_Phonems --
   -------------------------------

   function Need_Line_Between_Phonems
     (Root : P2G.Spiral_Model.Cursor; Root_GlyphName : String) return Boolean
   is
      Is_Start_Spiral : constant Boolean :=
        GlyphRep'Value (Root_GlyphName) = dot_start;

      Is_Start_Word : constant Boolean :=
        GlyphRep'Value (Root_GlyphName) = linedotline;

      Is_End_Branch : constant Boolean := P2G.Spiral_Model.Is_Leaf (Root);

   begin

      if not Is_End_Branch then

         declare

            --  If element has a vowel it will trigger a line even if 2nd child
            --  is a word-separator.
            Child : constant P2G.Spiral_Model.Cursor :=
              (if P2G.Spiral_Model.Child_Count (Root) > 1 then
                 P2G.Spiral_Model.Next_Sibling
                   (P2G.Spiral_Model.First_Child (Root))
               else P2G.Spiral_Model.First_Child (Root));

            E_Child : constant P2G.GlyphInfo :=
              P2G.Spiral_Model.Element (Child);

            Child_GlyphName : constant String :=
              S_U.To_String (E_Child.GlyphName);

            Is_End_Word : constant Boolean :=
              GlyphRep'Value (Child_GlyphName) = linedotline;

         begin
            return
              not (Is_Start_Spiral or else Is_Start_Word or else Is_End_Word);
         end;
      end if;

      return False;

   end Need_Line_Between_Phonems;

   ----------------------
   -- Get_Displacement --
   ----------------------

   procedure Get_Displacement_For_Branch
     (Element : P2G.GlyphInfo; dx_e, dy_e : in out Gdouble; Is_Child : Boolean)
   is

      r     : Gdouble := R_Poly;
      theta : Gdouble := 0.0; --  turn in anti-trigonometric sense. I think ?
   begin

      case GlyphRep'Value (S_U.To_String (Element.GlyphName)) is

         when bend =>

            if not Is_Child then
               r     := 1.2 * R_Poly;
               theta := 2.0;
            end if;

         when square =>
            theta := PI_2;

         when penta =>
            theta := PI_4 + 0.1;

         when hexa =>
            theta := PI_4 + 0.2;

         when squareline | squarebend =>

            r     := -R_Poly;
            theta := 3.0 * PI_4;

            if not Is_Child then
               theta := PI + 0.7;
            end if;

         when pentaline | pentabend =>

            if Is_Child then
               theta := -(PI_4 + 0.5);
            else
               r     := 1.4 * R_Poly;
               theta := PI_2 - 0.3;
            end if;

         when hexaline | hexabend =>

            if Is_Child then
               theta := -0.4;
            else
               theta := PI_3 - 0.5;
            end if;

         when heptaline | heptabend =>

            if Is_Child then
               theta := -PI_7 - 0.4;
            else
               theta := PI_2;
            end if;

         when squaresquare =>
            r     := 2.0 * R_Poly;
            theta := PI_2;

         when hexahexa =>

            if Is_Child then
               theta := -PI_6 - 0.5;
            else
               null;
            end if;

         when pentapenta =>

            if Is_Child then
               r     := -2.15 * R_Poly;
               theta := 3.0 * PI_4 + 0.4;
            else
               theta := PI_2 + 0.3;
            end if;

         when pentasquare =>

            theta := 0.5;

            if Is_Child then
               theta := -0.5;
            end if;

         when hexasquare | hexapenta =>

            if not Is_Child then
               theta := PI_3 - 0.2;
            end if;

         when heptasquare =>
            theta := -PI_7;

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

   procedure Draw_Branch_If_VN
     (Ctx : Cairo.Cairo_Context; Parent : P2G.GlyphInfo; Child : P2G.GlyphInfo;
      Xc, Yc, Xp, Yp : Gdouble)
   is

      dx_root, dy_root   : Gdouble := 0.0;
      dx_child, dy_child : Gdouble := 0.0;

   begin

      if Is_CS_V (Parent, Child) then

         Get_Displacement_For_Branch (Parent, dx_root, dy_root, False);
         Get_Displacement_For_Branch (Child, dx_child, dy_child, True);

         Cairo.Move_To (Ctx, Xp + dx_root, Yp + dy_root);
         Cairo.Line_To (Ctx, Xc + dx_child, Yc + dy_child);
         Cairo.Stroke (Ctx);

      end if;

   end Draw_Branch_If_VN;

end Draw_Utils;
