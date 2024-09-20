with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

with Draw_Glyphs;

with Draw_Utils;        use Draw_Utils;
with Draw_Spiral_Utils; use Draw_Spiral_Utils;
with Math;              use Math;

package body Draw_Spiral is

   package S_U renames Ada.Strings.Unbounded;

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   package DG renames Draw_Glyphs;
   package DSU renames Draw_Spiral_Utils;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Ctx  : in out Cairo.Cairo_Context; Element : P2G.GlyphInfo;
      I, N :        Gdouble; X, Y : in out Gdouble; state : Machine_State)
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

      X := state.Xb + (radius_var + Grad.dr) * Cos (theta_var + Grad.dtheta);
      Y := state.Yb - (radius_var + Grad.dr) * Sin (theta_var + Grad.dtheta);

      --  Grad := Calculate_Gradient (I, N);

      --  Grad  := (Grad.dr, Grad.dtheta);
      --  Tan_v := (Grad.dr, Grad.dtheta + PI_2);

      --  Cairo.Move_To (Ctx, X, Y);
      --  Cairo.Line_To
      --    (Ctx, X + 0.1 * (radius_var + Grad.dr) * Cos (theta_var - Grad.dtheta),
      --     Y - 0.1 * (radius_var + Grad.dr) * Sin (theta_var - Grad.dtheta));

      --  Cairo.Move_To (Ctx, X, Y);
      --  Cairo.Line_To
      --    (Ctx,
      --     X + 0.1 * (radius_var + Tan_v.dr) * Cos (theta_var - Tan_v.dtheta),
      --     Y - 0.1 * (radius_var + Tan_v.dr) * Sin (theta_var - Tan_v.dtheta));

      --  Cairo.Move_To (Ctx, X, Y);
      --  Cairo.Line_To (Ctx, X + 1.0, Y);
      --  Cairo.Stroke (Ctx);

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

      Transform (Ctx, Root_Elem, Depth_I, state.Depth_N, X_t, Y_t, state);
      --  Adjust_Element (Local_Angle, Depth_I, state.Depth_N);

      DG.Rotation_Around (Ctx, X_t, Y_t, Local_Angle);
      DG.Scaling_Around (Ctx, X_t, Y_t, Sx, Sx);

      DG.Choose_Glyph (Ctx, X_t, Y_t, GN_String_Root);

      DG.Scaling_Around (Ctx, X_t, Y_t, 1.0 / Sx, 1.0 / Sx);
      DG.Rotation_Around (Ctx, X_t, Y_t, -Local_Angle);

   end Draw_Spiral_Element;

   ----------------
   -- Draw_Lines --
   ----------------

   procedure Draw_Lines
     (Ctx           : in out Cairo.Cairo_Context;
      Parent, Child :        P2G.Spiral_Model.Cursor; state : Machine_State)
   is

      Parent_Elem : constant P2G.GlyphInfo :=
        P2G.Spiral_Model.Element (Parent);
      Child_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Child);

      Spiral_Side : constant Gdouble :=
        (if Parent_Elem.T = P2G.Vowel or else Parent_Elem.T = P2G.Numeral then
           1.0
         else 0.0);

      Depth_I : constant Gdouble :=
        Gdouble (P2G.Spiral_Model.Depth (Parent)) - Spiral_Side;
      --  The way multiway trees are implemented their "depth function" start
      --  with the root at the maximum depth (it has the most of children).

      Xp_t, Yp_t : Gdouble := 0.0;
      Xc_t, Yc_t : Gdouble := 0.0;

   begin

      Draw_Branch (Ctx, Parent_Elem, Child_Elem, Xc_t, Yc_t, Xp_t, Yp_t);

      if Need_Line_Between_Phonems (Parent, Child)
        and then Depth_I < state.Depth_N - 1.0
      then

         DSU.Line_Between_Words
           (Ctx, Parent_Elem, Child_Elem, Xc_t, Yc_t, Xp_t, Yp_t);
      end if;

   end Draw_Lines;

   -----------------
   -- Draw_Spiral --
   -----------------

   procedure Draw_Spiral
     (Ctx   : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      state : in out Machine_State)
   is

      Root_Elem  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Child_Elem : P2G.GlyphInfo;

      Current_Child : P2G.Spiral_Model.Cursor;

      I : Positive := 1;

   begin

      Draw_Spiral_Element (Ctx, Root, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            Child_Elem := P2G.Spiral_Model.Element (Current_Child);

            Draw_Lines (Ctx, Root, Current_Child, state);

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

            Grad.dr     := 0.0;
            Grad.dtheta := 0.0;

            if I mod 3 = 0 and then not (I = 21) then

               Cairo.Set_Source_Rgb (Ctx, 1.0, 0.0, 0.0);
               Grad := Calculate_Gradient (I_d, N_d, Is_Vowel => True);

            elsif I mod 7 = 0 then

               Cairo.Set_Source_Rgb (Ctx, 0.0, 0.0, 1.0);
               Grad := Calculate_Gradient (I_d, N_d, False);

            end if;

            X := Xb + (radius_var + Grad.dr) * Cos (theta_var - Grad.dtheta);
            Y := Yb - (radius_var + Grad.dr) * Sin (theta_var - Grad.dtheta);

            DG.Scaling_Around (Ctx, X, Y, Sx, Sx);
            DG.Ngone (Ctx, X, Y, 8);
            DG.Scaling_Around (Ctx, X, Y, 1.0 / Sx, 1.0 / Sx);

            Sx := Linearize_Scale (I_d, N_d);

         end;
      end loop;

   end Draw_Fibionnaci_Spiral;

end Draw_Spiral;
