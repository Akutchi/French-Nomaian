with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;

with Draw_Glyphs;
with Draw_Utils; use Draw_Utils;

package body Draw_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package DG renames Draw_Glyphs;

   -------------------------
   -- Draw_Spiral_Element --
   -------------------------

   procedure Draw_Spiral_Element
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state :        Machine_State)
   is

      Root_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      GN_String_Root : constant String := S_U.To_String (Root_Elem.GlyphName);

      X_t, Y_t    : Gdouble          := 0.0;
      Local_Angle : constant Gdouble := 0.0;

   begin

      Transform (Root_Elem, X_t, Y_t, 1.0, state.theta);

      DG.Rotation_Around (Ctx, X_t, Y_t, Local_Angle);

      DG.Choose_Glyph (Ctx, X_t, Y_t, GN_String_Root);

      DG.Rotation_Around (Ctx, X_t, Y_t, -Local_Angle);

   end Draw_Spiral_Element;

   ---------------
   -- Draw_CVSN --
   ---------------

   procedure Draw_CVSN
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state :        Machine_State)
   is
   begin

      Draw_Spiral_Element (Ctx, Root, state);

   end Draw_CVSN;

   -----------------
   -- Draw_Spiral --
   -----------------

   procedure Draw_Spiral
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state : in out Machine_State)
   is

      Current_Child : P2G.Spiral_Model.Cursor;
      Child_Elem    : P2G.GlyphInfo;

      theta_Parent : constant Gdouble := state.theta;
      I            : Positive         := 1;

   begin

      Draw_CVSN (Ctx, Root, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            state.theta := state.theta - state.Increment;
            Child_Elem  := P2G.Spiral_Model.Element (Current_Child);

            --  Draw_Branch
            --    (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp, Is_Unrolled);

            --  if Need_Line_Between_Phonems (Root, Current_Child) then
            --     DG.Line_Between_Words
            --       (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp, Di,
            --        Is_Unrolled);
            --  end if;

            --  if Need_Line_Between_Phonems (Root, Current_Child) then
            --     DG.Line_Between_Words
            --       (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp, Di,
            --        Is_Unrolled);
            --  end if;

            Draw_Spiral (Ctx, Current_Child, state);

            state.theta := theta_Parent;
            I           := I + 1;
            P2G.Spiral_Model.Next_Sibling (Current_Child);

         end loop;
      end if;

   end Draw_Spiral;

end Draw_Spiral;
