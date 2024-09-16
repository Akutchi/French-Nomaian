with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

with Draw_Glyphs;

with Ada.Text_IO;

package body Draw_Unrolled_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package DG renames Draw_Glyphs;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   -------------------------
   -- Draw_Spiral_Element --
   -------------------------

   procedure Draw_Spiral_Element
     (Ctx  : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      X, Y :        Gdouble; state : Machine_State)
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

      Is_V : constant Boolean := Element.T = P2G.Vowel;
      Is_N : constant Boolean := Element.T = P2G.Numeral;

   begin

      Draw_Spiral_Element (Ctx, Root, Xp, Yp, state);

      if not P2G.Spiral_Model.Is_Root (P2G.Spiral_Model.Parent (Root)) then

         Parent := P2G.Spiral_Model.Element (P2G.Spiral_Model.Parent (Root));

         if Is_CS_V (Parent, Element) then
            state.Xv := Xp + dx (Element.GlyphName, before) + Offset (Element);

         elsif Is_CS_N (Parent, Element) then
            state.Xn := Xp + dx (Element.GlyphName, before) + Offset (Element);

         elsif Is_V then
            state.Xv :=
              state.Xv + dx (Element.GlyphName, before) + Offset (Element);

         elsif Is_N then
            state.Xn :=
              state.Xn + dx (Element.GlyphName, before) + Offset (Element);

         end if;
      end if;

   end Draw_CVSN;

   ------------------------------
   -- Update_Child_Coordinates --
   ------------------------------

   procedure Update_Child_Coordinates
     (Root, Child : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp      : Gdouble; state : in out Machine_State)
   is

      Root_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      --  before / after the line that separates glyphs because some glyphs
      --  are not symmetric.
      dx_Root_Before : constant Gdouble := dx (Root_Elem.GlyphName, before);
      dx_Child_After : constant Gdouble := dx (Child_Elem.GlyphName, after);

      Vowel_Branching : constant Boolean :=
        Is_CS_V (Root_Elem, Child_Elem)
        or else Is_DX (Root_Elem, Child_Elem, P2G.Vowel);

      Numeral_Branching : constant Boolean :=
        Is_CS_N (Root_Elem, Child_Elem)
        or else Is_DX (Root_Elem, Child_Elem, P2G.Numeral);

      Dv, Dn : Gdouble;

   begin

      if Vowel_Branching then
         Dv :=
           (if Xp <= state.Xv then (state.Xv - Xp) + Offset_Leaf
            else Offset_Branch);
         Xc := Xp + Dv;
         Yc := Yp - dy_vn;

      elsif Numeral_Branching then

         Dn :=
           (if Xp <= state.Xn then (state.Xn - Xp) + Offset_Leaf
            else Offset_Branch);
         Xc := Xp + Dn;
         Yc := Yp + dy_vn;

      elsif Is_CX (Root_Elem, Child_Elem, P2G.Word_Separator) then
         Xc := Xp + dx_Root_Before;

      elsif Is_SX (Root_Elem, Child_Elem, P2G.Consonant) then
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

      Root_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      In_Branch : constant Boolean :=
        Is_CS_V (Root_Elem, Child_Elem) or else Is_CS_N (Root_Elem, Child_Elem)
        or else Is_DX (Root_Elem, Child_Elem, P2G.Vowel)
        or else Is_DX (Root_Elem, Child_Elem, P2G.Numeral);

   begin

      if not In_Branch then
         Yc := Yp;
      end if;

      if Child_Elem.T = P2G.Word_Separator then
         Xc := Xp + dx (Root_Elem.GlyphName, before);

      elsif Is_SX (Root_Elem, Child_Elem, P2G.Consonant) then
         Xc :=
           Xp + dx (Root_Elem.GlyphName, before) +
           dx (Child_Elem.GlyphName, after);

      elsif Child_Elem.T = P2G.Consonant then
         Xc :=
           Xp + dx (Root_Elem.GlyphName, before) + Line_Words_R_Poly +
           dx (Child_Elem.GlyphName, after);

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

      Is_Consonant_Or_Word_Sep : constant Boolean :=
        Parent_Elem.T /= P2G.Vowel and then Parent_Elem.T /= P2G.Numeral;

      Xp : constant Gdouble := X;
      Yp : Gdouble          := Y;

      Xc : Gdouble := X;
      Yc : Gdouble := Y;

      I : Positive := 1;

   begin

      if Is_Consonant_Or_Word_Sep then
         Update_Element_Coordinates (Parent_Elem, Yp, before);
      end if;

      Draw_CVSN (Ctx, Root, Xp, Yp, state);

      if Is_Consonant_Or_Word_Sep then
         Update_Element_Coordinates (Parent_Elem, Yp, after);
      end if;

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            Update_Child_Coordinates
              (Root, Current_Child, Xc, Yc, Xp, Yp, state);

            Child_Elem := P2G.Spiral_Model.Element (Current_Child);

            Draw_Branch (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp);

            Restore_To_Parent_Coordinates_If_CS
              (Root, Current_Child, Xc, Yc, Xp, Yp);

            if Need_Line_Between_Phonems (Root, Current_Child) then
               DG.Line_Between_Words
                 (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp);
            end if;

            Draw_Unrolled_Spiral (Ctx, Current_Child, Xc, Yc, state);

            P2G.Spiral_Model.Next_Sibling (Current_Child);
            I := I + 1;

         end loop;
      end if;

      if Parent_Elem.T = P2G.Numeral then
         state.Xn := 0.0; --  Because Prefix traversal V -> C/S -> N
      end if;

   end Draw_Unrolled_Spiral;

end Draw_Unrolled_Spiral;
