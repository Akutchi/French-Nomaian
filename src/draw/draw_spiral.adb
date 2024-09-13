with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;

with Draw_Glyphs;

package body Draw_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package DG renames Draw_Glyphs;

   ----------------
   -- Background --
   ----------------

   procedure Background (Ctx : in out Cairo.Cairo_Context) is
   begin

      Cairo.Set_Source_Rgb (Ctx, 0.98, 0.92, 0.84);
      Cairo.Rectangle (Ctx, 0.0, 0.0, 100.0, 100.0);
      Cairo.Fill (Ctx);
      Cairo.Set_Source_Rgb (Ctx, 0.06, 0.30, 0.55);
      Cairo.Set_Line_Width (Ctx, Line_Width);

   end Background;

   -------------------------
   -- Draw_Spiral_Element --
   -------------------------

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble)
   is

      E_Root : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      GN_String_Root : constant String := S_U.To_String (E_Root.GlyphName);

   begin

      case GlyphRep'Value (GN_String_Root) is

         when dot_start =>
            DG.Dot (Ctx, X, Y);

         when line =>
            DG.Line (Ctx, X, Y);

         when bend =>
            DG.Bend (Ctx, X, Y);

         when linedotline =>
            DG.Word_Separator (Ctx, X, Y);

         when square | penta | hexa | octa =>
            DG.Draw_Ngone (Ctx, GN_String_Root, X, Y, False, False);

         when squareline | pentaline | hexaline | heptaline | octaline =>
            DG.Draw_Ngone (Ctx, GN_String_Root, X, Y, True, False);

         when squarebend | pentabend | hexabend | heptabend | octabend =>
            DG.Draw_Ngone (Ctx, GN_String_Root, X, Y, False, True);

         when squaresquare =>
            DG.x2_Square (Ctx, X, Y);

         when pentapenta =>
            DG.x2_Penta (Ctx, X, Y);

         when hexahexa =>
            DG.x2_Hexa (Ctx, X, Y);

         when heptahepta =>
            DG.x2_Penta (Ctx, X, Y);

         when pentasquare =>
            DG.PentaSquare (Ctx, X, Y);

         when hexasquare =>
            DG.HexaSquare (Ctx, X, Y);

         when hexapenta =>
            DG.HexaPenta (Ctx, X, Y);

         when heptasquare =>
            DG.HeptaSquare (Ctx, X, Y);

         when heptapenta =>
            DG.HeptaPenta (Ctx, X, Y);

         when heptahexa =>
            DG.HeptaHexa (Ctx, X, Y);

         when others =>
            null;

      end case;

      if Need_Line_Between_Phonems (Root, GN_String_Root) then
         DG.Line_Between_Words (Ctx, X + dx (E_Root.GlyphName, before), Y);
      end if;

   end Draw_Spiral_Element;

   ---------------
   -- Draw_CVSN --
   ---------------

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State)
   is

      Parent  : constant P2G.GlyphInfo :=
        P2G.Spiral_Model.Element (P2G.Spiral_Model.Parent (Root));
      Element : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);

      Is_V : constant Boolean := Element.T = P2G.Vowel;
      Is_N : constant Boolean := Element.T = P2G.Numeral;

   begin

      Draw_Spiral_Element (Ctx, Root, Xp, Yp);

      if Is_CS_V (Parent, Element) then
         state.Xv := Xp + dx (Element.GlyphName, before) + 1.5 * R_Poly;

      elsif Is_V then
         state.Xv := state.Xv + dx (Element.GlyphName, before);

      elsif Is_CS_N (Parent, Element) then
         null; -- too implement later when everything works with vowels

      elsif Is_N then
         null;

      end if;

   end Draw_CVSN;

   ------------------------------
   -- Update_Child_Coordinates --
   ------------------------------

   procedure Update_Child_Coordinates
     (Root   : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp : Gdouble; state : Machine_State)
   is

      Child      : constant P2G.Spiral_Model.Cursor :=
        P2G.Spiral_Model.First_Child (Root);
      Child_Type : constant Character := P2G.Spiral_Model.Element (Child).T;

      E_Root  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      E_Child : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      --  before / after the line that separates glyphs because some glyphs
      --  are not symmetric.
      dx_Root_Before : constant Gdouble := dx (E_Root.GlyphName, before);
      dx_Child_After : constant Gdouble := dx (E_Child.GlyphName, after);

      Dv : Gdouble;

   begin

      if Is_CS_V (E_Root, E_Child) then

         Dv :=
           (if Xp <= state.Xv then (state.Xv - Xp) + Offset_Leaf
            else Offset_Branch);
         Xc := Xp + Dv;
         Yc := Yp - dy_vn;

      elsif Is_CX (E_Root, Child_Type, P2G.Word_Separator) then
         Xc := Xp + dx_Root_Before;

      elsif Is_SX (E_Root, Child_Type, P2G.Consonant) then
         Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;

      else
         Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;
      end if;

   end Update_Child_Coordinates;

   --------------------------------
   -- Update_Element_Coordinates --
   --------------------------------

   procedure Update_Element_Coordinates
     (Parent_Elem : P2G.GlyphInfo; Yp : in out Gdouble; dtype : dpos_Type)
   is
   begin
      Yp := Yp + dy (Parent_Elem.GlyphName, dtype);
   end Update_Element_Coordinates;

   -----------------------------------
   -- Restore_To_Parent_Coordinates --
   -----------------------------------

   procedure Restore_To_Parent_Coordinates_If_CS
     (Root, Child : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp      : Gdouble)
   is

      Child_Type : constant Character := P2G.Spiral_Model.Element (Child).T;

      E_Root  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      E_Child : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

   begin

      if not Is_CS_V (E_Root, E_Child) then
         Yc := Yp;
      end if;

      if E_Child.T = P2G.Word_Separator then
         Xc := Xp + dx (E_Root.GlyphName, before);

      elsif Is_SX (E_Root, Child_Type, P2G.Consonant) then
         Xc :=
           Xp + dx (E_Root.GlyphName, before) + dx (E_Child.GlyphName, after);

      elsif E_Child.T = P2G.Consonant then
         Xc :=
           Xp + dx (E_Root.GlyphName, before) + Line_Words_R_Poly +
           dx (E_Child.GlyphName, after);

      end if;

   end Restore_To_Parent_Coordinates_If_CS;

   --------------------------
   -- Draw_Unrolled_Spiral --
   --------------------------

   procedure Draw_Unrolled_Spiral
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble; state : in out Machine_State)
   is

      Current_Child : P2G.Spiral_Model.Cursor;

      Parent_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem  : P2G.GlyphInfo;

      Xp : constant Gdouble := X;
      Yp : Gdouble          := Y;

      Xc : Gdouble := X;
      Yc : Gdouble := Y;

      I : Positive := 1;

   begin

      if Parent_Elem.T /= P2G.Vowel then
         Update_Element_Coordinates (Parent_Elem, Yp, before);
      end if;

      Draw_CVSN (Ctx, Root, Xp, Yp, state);

      if Parent_Elem.T /= P2G.Vowel then
         Update_Element_Coordinates (Parent_Elem, Yp, after);
      end if;

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            Update_Child_Coordinates (Root, Xc, Yc, Xp, Yp, state);

            Child_Elem := P2G.Spiral_Model.Element (Current_Child);
            Draw_Branch_If_VN (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp);

            Restore_To_Parent_Coordinates_If_CS
              (Root, Current_Child, Xc, Yc, Xp, Yp);

            Draw_Unrolled_Spiral (Ctx, Current_Child, Xc, Yc, state);

            P2G.Spiral_Model.Next_Sibling (Current_Child);
            I := I + 1;

         end loop;
      end if;

   end Draw_Unrolled_Spiral;

end Draw_Spiral;
