with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Text_IO;

with Math_Constants; use Math_Constants;

package body Math is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   ---------------------
   -- Linearize_Scale --
   ---------------------

   function Linearize_Scale (x, N : Gdouble) return Gdouble is
   begin
      return (1.0 / (N - 1.0)) * (N - 0.5 * (x + 1.0));
   end Linearize_Scale;

   -------
   -- k --
   -------

   function k (N : Gdouble) return Gdouble is
   begin
      return 0.05 * Sqrt (N);
   end k;

   -------
   -- s --
   -------

   function s (N : Gdouble) return Gdouble is
   begin
      return 1.5 * N / 10.0 + 0.5;
   end s;

   -----------
   -- theta --
   -----------

   function theta (I, N : Gdouble) return Gdouble is
   begin
      return TWO_PI * (1.0 - 2.0 * k (N) * (I + 1.0) / N);
   end theta;

   ------------
   -- radius --
   ------------

   function radius (I, N : Gdouble) return Gdouble is
   begin
      return s (N) * Phi**(2.0 * theta (I, N) / PI);
   end radius;

   -----------------
   -- theta_prime --
   -----------------

   function theta_prime (N : Gdouble) return Gdouble is
   begin
      return -4.0 * PI * k (N) / N;

   end theta_prime;

   ------------------
   -- radius_prime --
   ------------------

   function radius_prime (I, N : Gdouble) return Gdouble is

      a : constant Gdouble := (2.0 * Log (e_d, Phi) / PI);
   begin

      return s (N) * a * (theta_prime (N)) * radius (theta (I, N), N);
   end radius_prime;

   ---------
   -- eps --
   ---------

   function eps (I, N : Gdouble) return Gdouble is
   begin
      return 0.55 * Exp (-0.05 * N) + 0.02 * (1.0 - 0.001 * I);
   end eps;

   ------------------------
   -- Calculate_Gradient --
   ------------------------

   function Calculate_Gradient
     (I, N : Gdouble; Is_Vowel : Boolean := False) return gradient
   is

      Gradient_Point : gradient;

      epsilon : constant Gdouble := eps (I, N);

      grad_r     : constant Gdouble := radius_prime (I, N);
      grad_theta : constant Gdouble := theta_prime (N);

      Spiral_Side : constant Gdouble := (if Is_Vowel then -1.0 else 1.0);

   begin

      Gradient_Point.dr     := Spiral_Side * epsilon * grad_r;
      Gradient_Point.dtheta := Spiral_Side * epsilon * grad_theta;

      return Gradient_Point;

   end Calculate_Gradient;

   ----------
   -- norm --
   ----------

   function norm (Point : vector) return Gdouble is
   begin

      return Sqrt (Point.p1**2 + Point.p2**2);
   end norm;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (u : vector) return vector is

      u_n : Gdouble;
      v   : vector := (0.0, 0.0);

   begin

      u_n := norm (u);

      if u_n >= 0.001 then

         v.p1 := u.p1 / u_n;
         v.p2 := u.p2 / u_n;

         return v;

      else
         return (0.0, 0.0);
      end if;

   end Normalize;

   ---------
   -- dot --
   ---------

   function dot (u : vector; v : vector) return Gdouble is
   begin

      return u.p1 * v.p1 + u.p2 * v.p2;

   end dot;

   --------------------
   -- Adjust_Element --
   --------------------

   procedure Adjust_Element (Angle : in out Gdouble; I, N : Gdouble) is

      Element_vector : constant vector := (1.0, 0.0);

      Grad    : constant gradient := Calculate_Gradient (I, N);
      Tangent : constant vector   := Normalize ((Grad.dr, Grad.dtheta - PI_2));

      scalar : Gdouble;

   begin

      scalar := dot (Tangent, Element_vector);
      Angle  := Arccos (scalar);

      --  Ada.Text_IO.Put_Line (Gdouble'Image (Angle));

   end Adjust_Element;

end Math;
