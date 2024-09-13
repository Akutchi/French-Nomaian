with Ada.Containers; use Ada.Containers;

package body Draw_Utils is

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
            return 0.85 * R_Poly;

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
                  return 2.0 * R_Poly;
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
            return 0.0;

         when hexapenta =>

            case dp is
               when before =>
                  return 1.95 * R_Poly;
               when after =>
                  return R_Poly;
            end case;
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

   end dx;

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

end Draw_Utils;
