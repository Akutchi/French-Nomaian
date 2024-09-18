with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

with Draw_Glyphs;

with Math; use Math;

with Ada.Text_IO;

package body Draw_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   package DG renames Draw_Glyphs;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Element : P2G.GlyphInfo; I, N : Gdouble; X, Y : in out Gdouble;
      state   : Machine_State)
   is

      radius_var : constant Gdouble := radius (I, N);
      theta_var  : constant Gdouble := theta (I, N);

      Grad : gradient;

   begin

      if Element.T = P2G.Vowel then
         Grad := Calculate_Gradient (I, N, Is_Vowel => True);
      elsif Element.T = P2G.Numeral then
         Grad := Calculate_Gradient (I, N, Is_Vowel => False);
      end if;

      X := state.Xb + (radius_var + Grad.dx) * Cos (theta_var - Grad.dy);
      Y := state.Yb - (radius_var + Grad.dx) * Sin (theta_var - Grad.dy);

   end Transform;

   -------------------------
   -- Draw_Spiral_Element --
   -------------------------

   procedure Draw_Spiral_Element
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state :        Machine_State)
   is

      Root_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      GN_String_Root : constant String := S_U.To_String (Root_Elem.GlyphName);

      X_t, Y_t    : Gdouble := 0.0;
      Local_Angle : Gdouble := 0.0;

      Spiral_Side : constant Gdouble :=
        (if Root_Elem.T = P2G.Vowel or else Root_Elem.T = P2G.Numeral then 1.0
         else 0.0);

      Depth_I : constant Gdouble :=
        Gdouble (P2G.Spiral_Model.Depth (Root)) - Spiral_Side;
      --  The way multiway trees are implemented their "depth function" start
      --  with the root at the maximum depth (it has the most of children).

      Sx : constant Gdouble := Linearize_Scale (Depth_I, state.Depth_N);

   begin

      Ada.Text_IO.Put_Line (Gdouble'Image (state.Depth_N));

      if GN_String_Root = "line" then

         Local_Angle := theta (Depth_I, state.Depth_N);
      end if;

      Transform (Root_Elem, Depth_I, state.Depth_N, X_t, Y_t, state);

      DG.Rotation_Around (Ctx, X_t, Y_t, Local_Angle);
      DG.Scaling_Around (Ctx, X_t, Y_t, Sx, Sx);

      DG.Choose_Glyph (Ctx, X_t, Y_t, GN_String_Root);

      DG.Scaling_Around (Ctx, X_t, Y_t, 1.0 / Sx, 1.0 / Sx);
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

   --  Child_Elem  := P2G.Spiral_Model.Element (Current_Child);

   --  Draw_Branch
   --    (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp, Is_Unrolled);

   --  if Need_Line_Between_Phonems (Root, Current_Child) then
   --     DG.Line_Between_Words
   --       (Ctx, Parent_Elem, Child_Elem, Xc, Yc, Xp, Yp, Di,
   --        Is_Unrolled);
   --  end if;

   procedure Draw_Spiral
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state : in out Machine_State)
   is

      Current_Child : P2G.Spiral_Model.Cursor;
      I             : Positive := 1;

   begin

      Draw_CVSN (Ctx, Root, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            Draw_Spiral (Ctx, Current_Child, state);

            I := I + 1;
            P2G.Spiral_Model.Next_Sibling (Current_Child);

         end loop;
      end if;

   end Draw_Spiral;

   ----------------------------
   -- Draw_Fibionnaci_Spiral --
   ----------------------------

   procedure Draw_Fibionnaci_Spiral
     (Ctx : in out Cairo.Cairo_Context; Xb, Yb : Gdouble; N : Positive)
   is

      N_d : constant Gdouble := Gdouble (N);
      Sx  : Gdouble          := 1.0;

   begin

      for I in 1 .. N loop

         Cairo.Set_Source_Rgb
           (Ctx, 1.0 / Gdouble (I), 1.0 / Gdouble (I), 1.0 / Gdouble (I));

         declare

            I_d        : constant Gdouble := Gdouble (I);
            radius_var : constant Gdouble := radius (I_d, N_d);
            theta_var  : constant Gdouble := theta (I_d, N_d);

            X, Y : Gdouble;
            Grad : gradient;

         begin

            Grad.dx := 0.0;
            Grad.dy := 0.0;

            if I mod 3 = 0 and then not (I = 21) then

               Cairo.Set_Source_Rgb (Ctx, 1.0, 0.0, 0.0);
               Grad := Calculate_Gradient (I_d, N_d, Is_Vowel => True);

            elsif I mod 7 = 0 then

               Cairo.Set_Source_Rgb (Ctx, 0.0, 0.0, 1.0);
               Grad := Calculate_Gradient (I_d, N_d, False);

            end if;

            X := Xb + radius_var * Cos (theta_var) + Grad.dx;
            Y := Yb - radius_var * Sin (theta_var) - Grad.dy;

            DG.Scaling_Around (Ctx, X, Y, Sx, Sx);
            DG.Ngone (Ctx, X, Y, 8);
            DG.Scaling_Around (Ctx, X, Y, 1.0 / Sx, 1.0 / Sx);

            Sx := Linearize_Scale (I_d, N_d);

         end;
      end loop;

   end Draw_Fibionnaci_Spiral;

end Draw_Spiral;
