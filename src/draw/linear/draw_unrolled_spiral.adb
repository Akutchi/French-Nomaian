with Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

with Draw_Glyphs;
with Draw_Unrolled_Utils;
with Draw_Utils;

with Draw_Constants; use Draw_Constants;

package body Draw_Unrolled_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package DG renames Draw_Glyphs;
   package DUU renames Draw_Unrolled_Utils;
   package DU renames Draw_Utils;

   -------------------------
   -- Draw_Spiral_Element --
   -------------------------

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble)
   is

      Root_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      GN_String_Root : constant String := S_U.To_String (Root_Elem.GlyphName);

   begin

      DG.Choose_Glyph (Ctx, X, Y, GN_String_Root);

   end Draw_Spiral_Element;

   ---------------
   -- Draw_CVSN --
   ---------------

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State)
   is

      Parent  : P2G.GlyphInfo;
      Element : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);

      --  before / after the word separator  because some glyphs
      --  are not symmetric.
      dx_Elem_Before : constant Gdouble :=
        DUU.dx_For_Word_Separator (Element.GlyphName, before);

      Is_V : constant Boolean := Element.T = P2G.Vowel;
      Is_N : constant Boolean := Element.T = P2G.Numeral;

   begin

      Draw_Spiral_Element (Ctx, Root, Xp, Yp);

      if not P2G.Spiral_Model.Is_Root (P2G.Spiral_Model.Parent (Root)) then

         Parent := P2G.Spiral_Model.Element (P2G.Spiral_Model.Parent (Root));

         if DU.Is_CS_V (Parent, Element) then
            state.Xv := Xp + dx_Elem_Before + DUU.Branch_Offset (Element);

         elsif DU.Is_CS_N (Parent, Element) then
            state.Xn := Xp + dx_Elem_Before + DUU.Branch_Offset (Element);

         elsif Is_V then
            state.Xv :=
              state.Xv + dx_Elem_Before + DUU.Branch_Offset (Element);

         elsif Is_N then
            state.Xn :=
              state.Xn + dx_Elem_Before + DUU.Branch_Offset (Element);

         end if;
      end if;

   end Draw_CVSN;

   ------------------------------
   -- Update_Branch_Coordinates --
   ------------------------------

   procedure Update_Branch_Coordinates
     (Root, Child :        P2G.Spiral_Model.Cursor; Xp, Yp : Gdouble;
      state       : in out Machine_State)
   is

      Root_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      --  before / after the the word separator because some glyphs
      --  are not symmetric.
      dx_Root_Before : constant Gdouble :=
        DUU.dx_For_Word_Separator (Root_Elem.GlyphName, before);
      dx_Child_After : constant Gdouble :=
        DUU.dx_For_Word_Separator (Child_Elem.GlyphName, after);

      Vowel_Branching : constant Boolean :=
        DU.Is_CS_V (Root_Elem, Child_Elem)
        or else DU.Is_DX (Root_Elem, Child_Elem, P2G.Vowel);

      Numeral_Branching : constant Boolean :=
        DU.Is_CS_N (Root_Elem, Child_Elem)
        or else DU.Is_DX (Root_Elem, Child_Elem, P2G.Numeral);

      Dv, Dn : Gdouble;

   begin

      if Vowel_Branching then
         Dv       :=
           (if Xp <= state.Xv then (state.Xv - Xp) + Offset_Leaf
            else Offset_Branch);
         state.Xc := Xp + Dv;
         state.Yc := Yp - dy_vn;

      elsif Numeral_Branching then

         Dn       :=
           (if Xp <= state.Xn then (state.Xn - Xp) + Offset_Leaf
            else Offset_Branch);
         state.Xc := Xp + Dn;
         state.Yc := Yp + dy_vn;

      elsif DU.Is_CX (Root_Elem, Child_Elem, P2G.Word_Separator) then
         state.Xc := Xp + dx_Root_Before;

      elsif DU.Is_SX (Root_Elem, Child_Elem, P2G.Consonant) then
         state.Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;

      else
         state.Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;
      end if;

   end Update_Branch_Coordinates;

   -----------------------------------
   -- Restore_To_Parent_Coordinates --
   -----------------------------------

   procedure Restore_To_Parent_Coordinates_If_CS
     (Root, Child :        P2G.Spiral_Model.Cursor; Xp, Yp : Gdouble;
      state       : in out Machine_State)
   is

      Root_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      In_Branch : constant Boolean :=
        DU.Is_CS_V (Root_Elem, Child_Elem)
        or else DU.Is_CS_N (Root_Elem, Child_Elem)
        or else DU.Is_DX (Root_Elem, Child_Elem, P2G.Vowel)
        or else DU.Is_DX (Root_Elem, Child_Elem, P2G.Numeral);

      dx_Root_Before  : constant Gdouble :=
        DUU.dx_For_Word_Separator (Root_Elem.GlyphName, before);
      dx_Child_After  : constant Gdouble :=
        DUU.dx_For_Word_Separator (Child_Elem.GlyphName, after);
      dy_Child_Before : constant Gdouble :=
        DUU.dy_For_Word_Separator (Child_Elem.GlyphName, before);
      dy_Root_After   : constant Gdouble :=
        DUU.dy_For_Word_Separator (Root_Elem.GlyphName, after);

   begin

      if not In_Branch then
         state.Yc := Yp;
      end if;

      if Child_Elem.T = P2G.Word_Separator then
         state.Xc := Xp + dx_Root_Before;

      elsif DU.Is_SX (Root_Elem, Child_Elem, P2G.Consonant) then
         state.Xc := Xp + dx_Root_Before + dx_Child_After;

      elsif Child_Elem.T = P2G.Consonant then
         state.Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;
      end if;

      if DU.Is_SX (Root_Elem, Child_Elem, 's') then
         state.Yc := Yp + 0.4 * R_Poly;

      elsif DU.Is_SX (Root_Elem, Child_Elem, 'c') then
         state.Yc := Yp + dy_Child_Before;

      elsif DU.Is_CX (Root_Elem, Child_Elem, 's') then
         state.Yc := Yp + dy_Root_After;

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
      Yp : constant Gdouble := Y;

      I : Positive := 1;

   begin

      state.Xc := X;
      state.Yc := Y;

      Draw_CVSN (Ctx, Root, Xp, Yp, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            Update_Branch_Coordinates (Root, Current_Child, Xp, Yp, state);

            Child_Elem := P2G.Spiral_Model.Element (Current_Child);

            DUU.Draw_Branch
              (Ctx, Parent_Elem, Child_Elem, state.Xc, state.Yc, Xp, Yp);

            Restore_To_Parent_Coordinates_If_CS
              (Root, Current_Child, Xp, Yp, state);

            if DU.Need_Line_Between_Phonems (Root, Current_Child) then
               DUU.Line_Between_Words
                 (Ctx, Parent_Elem, Child_Elem, state.Xc, state.Yc, Xp, Yp);
            end if;

            Draw_Unrolled_Spiral
              (Ctx, Current_Child, state.Xc, state.Yc, state);

            P2G.Spiral_Model.Next_Sibling (Current_Child);
            I := I + 1;

         end loop;
      end if;

      if Parent_Elem.T = P2G.Numeral then
         state.Xn := 0.0; --  Because Prefix traversal V -> C/S -> N
      end if;

   end Draw_Unrolled_Spiral;

end Draw_Unrolled_Spiral;
