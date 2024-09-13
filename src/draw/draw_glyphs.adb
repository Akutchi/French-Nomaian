with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Containers; use Ada.Containers;

with Ada.Text_IO;

package body Draw_Glyphs is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   ----------------
   -- Background --
   ----------------

   procedure Background (Ctx : Cairo.Cairo_Context) is
   begin

      Cairo.Set_Source_Rgb (Ctx, 0.98, 0.92, 0.84);
      Cairo.Rectangle (Ctx, 0.0, 0.0, 100.0, 100.0);
      Cairo.Fill (Ctx);
      Cairo.Set_Source_Rgb (Ctx, 0.06, 0.30, 0.55);
      Cairo.Set_Line_Width (Ctx, Line_Width);

   end Background;

   ---------------------
   -- Rotation_Around --
   ---------------------

   procedure Rotation_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Angle : Gdouble)
   is
   begin
      Cairo.Translate (Ctx, X, Y);
      Cairo.Rotate (Ctx, Angle);
      Cairo.Translate (Ctx, -X, -Y);

   end Rotation_Around;

   --------------------
   -- Scaling_Around --
   --------------------

   procedure Scaling_Around
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; Sx, Sy : Gdouble)
   is
   begin
      Cairo.Translate (Ctx, X, Y);
      Cairo.Scale (Ctx, Sx, Sy);
      Cairo.Translate (Ctx, -X, -Y);

   end Scaling_Around;

   ---------
   -- Dot --
   ---------

   procedure Dot (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Arc (Ctx, X, Y, R_Dot, 0.0, 360.0);
      Cairo.Fill (Ctx);
      Cairo.Stroke (Ctx);

   end Dot;

   ------------------------
   -- Line_Between_Words --
   ------------------------

   procedure Line_Between_Words
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble)
   is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + Line_Words_R_Poly, Y);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + Line_Words_R_Poly, Y);

   end Line_Between_Words;

   ----------
   -- Line --
   ----------

   procedure Line (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + Line_Glyph_R_Poly, Y);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + Line_Glyph_R_Poly, Y);

   end Line;

   ----------
   -- Bend --
   ----------

   procedure Bend (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Rotation_Around (Ctx, X + R_Poly, Y, PI);

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.5 * R_Poly, Y + R_Poly);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + 1.5 * R_Poly, Y + R_Poly);

      Rotation_Around (Ctx, X + R_Poly, Y, -1.0 * PI);

   end Bend;

   --------------------
   -- Word_Separator --
   --------------------

   procedure Word_Separator (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble)
   is
   begin

      Dot (Ctx, X, Y);

      Cairo.Move_To (Ctx, X, Y);
      Cairo.Line_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 2.0 * R_Poly, Y + 0.4 * R_Poly);
      Cairo.Stroke (Ctx);

      Dot (Ctx, X + R_Poly, Y);
      Dot (Ctx, X + 2.0 * R_Poly, Y + 0.4 * R_Poly);

   end Word_Separator;

   -----------
   -- Ngone --
   -----------

   procedure Ngone
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive;
      r   :        Gdouble := R_Poly)
   is

      Start_Angle : constant Gdouble := 0.0;
      Increment   : constant Gdouble := Gdouble (2.0 * Ada.Numerics.Pi / N);

      theta : Gdouble := Start_Angle;

      I : Positive := 1;

   begin

      Cairo.Move_To (Ctx, X, Y);
      loop

         declare

            X1 : constant Gdouble := X + r * Cos (theta);
            Y1 : constant Gdouble := Y + r * Sin (theta);

            X2 : constant Gdouble := X + r * Cos (theta + Increment);
            Y2 : constant Gdouble := Y + r * Sin (theta + Increment);

         begin

            exit when I > N + 1;

            Dot (Ctx, X1, Y1);

            Cairo.Move_To (Ctx, X1, Y1);
            Cairo.Line_To (Ctx, X2, Y2);
            Cairo.Stroke (Ctx);

            Dot (Ctx, X2, Y2);

            theta := theta + Increment;
            I     := I + 1;

         end;
      end loop;

   end Ngone;

   ---------------
   -- NgoneLine --
   ---------------

   procedure NgoneLine
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin
      Rotation_Around (Ctx, X, Y, -1.0 * PI / Gdouble (N));
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.9 * R_Poly, Y);
      Cairo.Stroke (Ctx);
      Rotation_Around (Ctx, X, Y, 1.0 * PI / Gdouble (N));

   end NgoneLine;

   ---------------
   -- NgoneBend --
   ---------------

   procedure NgoneBend
     (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble; N : Positive)
   is
   begin

      Rotation_Around (Ctx, X, Y, -1.0 * PI / Gdouble (N));
      Ngone (Ctx, X, Y, N);
      Cairo.Move_To (Ctx, X + R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.9 * R_Poly, Y);
      Cairo.Move_To (Ctx, X + 1.75 * R_Poly, Y);
      Cairo.Line_To (Ctx, X + 1.75 * R_Poly, Y + 0.5 * R_Poly);
      Cairo.Stroke (Ctx);
      Rotation_Around (Ctx, X, Y, 1.0 * PI / Gdouble (N));

   end NgoneBend;

   -----------------
   -- PentaSquare --
   -----------------

   procedure PentaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.16;
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 1.0 * R_Poly, Y - 0.8 * R_Poly, 4, 0.8 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end PentaSquare;

   ----------------
   -- HexaSquare --
   ----------------

   procedure HexaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.26;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + R_Poly, Y - 0.75 * R_Poly, 4, 0.7 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaSquare;

   ---------------
   -- HexaPenta --
   ---------------

   procedure HexaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.74;
   begin

      Ngone (Ctx, X, Y, 6);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.7 * R_Poly, Y - 0.8 * R_Poly, 5, 0.8 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HexaPenta;

   -----------------
   -- HeptaSquare --
   -----------------

   procedure HeptaSquare (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.33;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 1.0 * R_Poly, Y - 0.6 * R_Poly, 4, 0.6 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaSquare;

   ----------------
   -- HeptaPenta --
   ----------------

   procedure HeptaPenta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 0.80;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.8 * R_Poly, Y - 0.7 * R_Poly, 5, 0.7 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaPenta;

   ---------------
   -- HeptaHexa --
   ---------------

   procedure HeptaHexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is

      rad : constant Gdouble := 1.1;
   begin

      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X + R_Poly, Y, rad);
      Ngone (Ctx, X + 0.6 * R_Poly, Y - 0.8 * R_Poly, 6, 0.9 * R_Poly);
      Rotation_Around (Ctx, X + R_Poly, Y, -rad);

   end HeptaHexa;

   ---------------
   -- x2_Square --
   ---------------

   procedure x2_Square (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 4);
      Ngone (Ctx, X + R_Poly, Y + R_Poly, 4);
   end x2_Square;

   ----------------
   -- x2_Penta --
   ----------------

   procedure x2_Penta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin

      Ngone (Ctx, X, Y, 5);
      Rotation_Around (Ctx, X, Y, 2.90 * TWO_PI);
      Ngone (Ctx, X - 1.315 * R_Poly, Y - 0.95 * R_Poly, 5);
      Rotation_Around (Ctx, X, Y, -2.90 * TWO_PI);

   end x2_Penta;

   --------------
   -- x2_Hexa --
   --------------

   procedure x2_Hexa (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 6);
      Ngone (Ctx, X - 1.5 * R_Poly, Y - 0.87 * R_Poly, 6);
   end x2_Hexa;

   ----------------
   -- x2_Hepta --
   ----------------

   procedure x2_Hepta (Ctx : in out Cairo.Cairo_Context; X, Y : Gdouble) is
   begin
      Ngone (Ctx, X, Y, 7);
      Rotation_Around (Ctx, X, Y, 2.93 * TWO_PI);
      Ngone (Ctx, X - 1.62 * R_Poly, Y - 0.79 * R_Poly, 7);
      Rotation_Around (Ctx, X, Y, -2.93 * TWO_PI);

   end x2_Hepta;

   ----------------
   -- Draw_Ngone --
   ----------------

   procedure Draw_Ngone
     (Ctx : in out Cairo.Cairo_Context; GlyphName : String; X, Y : Gdouble;
      Has_Line, Has_Bend :        Boolean)
   is
      Sides : Positive;

      Sliced_GlyphName : constant String :=
        GlyphName (GlyphName'First .. GlyphName'Last - 4);

      GN_String : constant String :=
        (if Has_Line or else Has_Bend then Sliced_GlyphName else GlyphName);
   begin

      case GlyphRep'Value (GN_String) is
         when square =>
            Sides := 4;
         when penta =>
            Sides := 5;
         when hexa =>
            Sides := 6;
         when hepta =>
            Sides := 7;
         when octa =>
            Sides := 8;
         when others => --  Should not happen
            Sides := 4;
      end case;

      if Has_Line then
         NgoneLine (Ctx, X, Y, Sides);

      elsif Has_Bend then
         NgoneBend (Ctx, X, Y, Sides);

      else
         Ngone (Ctx, X, Y, Sides);
      end if;

   end Draw_Ngone;

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
            Dot (Ctx, X, Y);

         when line =>
            Line (Ctx, X, Y);

         when bend =>
            Bend (Ctx, X, Y);

         when linedotline =>
            Word_Separator (Ctx, X, Y);

         when square | penta | hexa | octa =>
            Draw_Ngone (Ctx, GN_String_Root, X, Y, False, False);

         when squareline | pentaline | hexaline | heptaline | octaline =>
            Draw_Ngone (Ctx, GN_String_Root, X, Y, True, False);

         when squarebend | pentabend | hexabend | heptabend | octabend =>
            Draw_Ngone (Ctx, GN_String_Root, X, Y, False, True);

         when squaresquare =>
            x2_Square (Ctx, X, Y);

         when pentapenta =>
            x2_Penta (Ctx, X, Y);

         when hexahexa =>
            x2_Hexa (Ctx, X, Y);

         when heptahepta =>
            x2_Penta (Ctx, X, Y);

         when pentasquare =>
            PentaSquare (Ctx, X, Y);

         when hexasquare =>
            HexaSquare (Ctx, X, Y);

         when hexapenta =>
            HexaPenta (Ctx, X, Y);

         when heptasquare =>
            HeptaSquare (Ctx, X, Y);

         when heptapenta =>
            HeptaPenta (Ctx, X, Y);

         when heptahexa =>
            HeptaHexa (Ctx, X, Y);

         when others =>
            null;

      end case;

      if Need_Line_Between_Phonems (Root, GN_String_Root) then
         Line_Between_Words (Ctx, X + dx (E_Root.GlyphName, before), Y);
      end if;

   end Draw_Spiral_Element;

   ------------------
   -- Update_Child --
   ------------------

   procedure Update_Child
     (Root   : P2G.Spiral_Model.Cursor; Xc, Yc : in out Gdouble;
      Xp, Yp : Gdouble)
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

   begin

      if Is_CX (E_Root, Child_Type, 's') then
         Xc := Xp + dx_Root_Before + Line_Words_R_Poly;

      elsif Is_SX (E_Root, Child_Type, 'c') then
         Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;

      else
         Xc := Xp + dx_Root_Before + Line_Words_R_Poly + dx_Child_After;
      end if;

   end Update_Child;

   ---------------
   -- Draw_CVSN --
   ---------------

   procedure Draw_CVSN
     (Ctx    : in out Cairo.Cairo_Context; Root : P2G.Spiral_Model.Cursor;
      Xp, Yp :        Gdouble; state : in out Machine_State)
   is

      P  : constant P2G.GlyphInfo :=
        P2G.Spiral_Model.Element (P2G.Spiral_Model.Parent (Root));
      E  : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      Dv : Gdouble;

   begin
      if (P.T = 'c' or else P.T = 's') and then E.T = 'v' then

         Dv :=
           (if Xp <= state.Xv then (state.Xv - Xp) + 0.2 * R_Poly else 0.0);

         Draw_Spiral_Element (Ctx, Root, Xp + Dv, Yp - dy);
         state.Xv := Xp + dx (E.GlyphName, before) + 1.5 * R_Poly;

      elsif E.T = 'v' then

         Draw_Spiral_Element (Ctx, Root, Xp, Yp - dy);
         state.Xv := state.Xv + dx (E.GlyphName, before);

      elsif (P.T = 'c' or else P.T = 's') and then E.T = 'n' then
         Draw_Spiral_Element (Ctx, Root, Xp, Yp + dy);

      elsif E.T = 'n' then
         Draw_Spiral_Element (Ctx, Root, Xp, Yp + dy);

      else
         Draw_Spiral_Element (Ctx, Root, Xp, Yp);
      end if;
   end Draw_CVSN;

   --------------------------
   -- Draw_Unrolled_Spiral --
   --------------------------

   procedure Draw_Unrolled_Spiral
     (Ctx   : in out Cairo.Cairo_Context; X, Y : Gdouble;
      state : in out Machine_State; Root : P2G.Spiral_Model.Cursor)
   is

      Current_Child : P2G.Spiral_Model.Cursor;

      Xp : constant Gdouble := X;
      Yp : constant Gdouble := Y;

      Xc : Gdouble := X;
      Yc : Gdouble := Y;

      I : Positive := 1;

      E   : constant P2G.GlyphInfo := P2G.Spiral_Model.Element (Root);
      E_C : P2G.GlyphInfo;

   begin

      Draw_CVSN (Ctx, Root, Xp, Yp, state);

      if not P2G.Spiral_Model.Is_Leaf (Root) then

         Current_Child := P2G.Spiral_Model.First_Child (Root);

         while Count_Type (I) <= P2G.Spiral_Model.Child_Count (Root) loop

            E_C := P2G.Spiral_Model.Element (Current_Child);

            Update_Child (Root, Xc, Yc, Xp, Yp);

            if E_C.T = 's' then
               Xc := Xp + dx (E.GlyphName, before);

            elsif E.T = 's' and then E_C.T = 'c' then
               Xc := Xp + dx (E.GlyphName, before) + dx (E_C.GlyphName, after);

            elsif E_C.T = 'c' then
               Xc :=
                 Xp + dx (E.GlyphName, before) + Line_Words_R_Poly +
                 dx (E_C.GlyphName, after);

            end if;

            Draw_Unrolled_Spiral (Ctx, Xc, Yc, state, Current_Child);

            P2G.Spiral_Model.Next_Sibling (Current_Child);
            I := I + 1;

         end loop;
      end if;

   end Draw_Unrolled_Spiral;

end Draw_Glyphs;
