with Ada.Strings.Unbounded;

with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Generic_Elementary_Functions;

with Draw_Glyphs;

with Math;           use Math;
with Math_Constants; use Math_Constants;

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
     (Element : P2G.GlyphInfo; X, Y : in out Gdouble; state : Machine_State)
   is
      Xb : constant Gdouble := 100.0;
      Yb : constant Gdouble := 100.0;

      a : constant Gdouble := 5.0;

      r : constant Gdouble := a * Phi**(2.0 * state.theta / PI);

      Spiral_Side : constant Gdouble :=
        (if Element.T = P2G.Vowel then 5.0
         elsif Element.T = P2G.Numeral then -5.0 else 0.0);

      Shift : constant Gdouble :=
        (if Element.T = P2G.Vowel or else Element.T = P2G.Numeral then 2.0
         else 0.0);

   begin

      X := Xb + r * Cos (state.theta) + Shift;
      Y := Yb - r * Sin (state.theta) + Spiral_Side;

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

   begin

      if GN_String_Root = "line" then

         Local_Angle := PI_4 + 0.5;
      end if;

      Transform (Root_Elem, X_t, Y_t, state);

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

      Root_Elem : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      --  Child_Elem : P2G.GlyphInfo;

      X_t, Y_t : Gdouble := 0.0;

      theta_Parent : constant Gdouble := state.theta;
      I            : Positive         := 1;

   begin

      Draw_CVSN (Ctx, Root, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            state.theta := state.theta - state.Increment;

            --  Child_Elem  := P2G.Spiral_Model.Element (Current_Child);

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

            Transform (Root_Elem, X_t, Y_t, state);

            state.theta := theta_Parent;
            I           := I + 1;
            P2G.Spiral_Model.Next_Sibling (Current_Child);

         end loop;
      end if;

   end Draw_Spiral;

   ----------------------------
   -- Draw_Fibionnaci_Spiral --
   ----------------------------

   procedure Draw_Fibionnaci_Spiral
     (Ctx : in out Cairo.Cairo_Context; Xb, Yb, Start_Angle : Gdouble;
      N   :        Positive)
   is
      Sx  : Gdouble          := 1.0;
      N_d : constant Gdouble := Gdouble (N);

   begin

      for I in 1 .. N loop

         Cairo.Set_Source_Rgb
           (Ctx, 1.0 / Gdouble (I), 1.0 / Gdouble (I), 1.0 / Gdouble (I));

         declare

            I_d : constant Gdouble := Gdouble (I);

            k         : constant Gdouble := 0.4;
            end_space : constant Gdouble := 1.0 - TWO_PI;

            theta_var : constant Gdouble :=
              theta (I_d, N_d, Start_Angle, a, k);

            X, Y        : Gdouble;
            dr, d_theta : Gdouble := 0.0;

            eps        : constant Gdouble := 2.0;
            grad_r     : constant Gdouble :=
              radius_prime (I_d, N_d, Start_Angle, end_space, k);
            grad_theta : constant Gdouble :=
              theta_prime (I_d, N_d, end_space, k);

         begin

            if I mod 3 = 0 and then not (I = 21) then
               Cairo.Set_Source_Rgb (Ctx, 1.0, 0.0, 0.0);

               dr      := eps * grad_r;
               d_theta := eps * grad_theta;

            elsif I mod 7 = 0 then
               Cairo.Set_Source_Rgb (Ctx, 0.0, 0.0, 1.0);

               dr      := -eps * grad_r;
               d_theta := -eps * grad_theta;

            end if;

            X := Xb + (radius (theta_var) + dr) * Cos (theta_var + d_theta);
            Y := Yb - (radius (theta_var) + dr) * Sin (theta_var + d_theta);

            DG.Scaling_Around (Ctx, X, Y, Sx, Sx);
            DG.Ngone (Ctx, X, Y, 8);
            DG.Scaling_Around (Ctx, X, Y, 1.0 / Sx, 1.0 / Sx);

            Sx := Linearize_Scale (I_d, N_d);

         end;
      end loop;

   end Draw_Fibionnaci_Spiral;

end Draw_Spiral;
