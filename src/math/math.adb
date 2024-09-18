with Ada.Numerics.Generic_Elementary_Functions;

with Math_Constants; use Math_Constants;

with Ada.Text_IO;

package body Math is

   package Functions is new Ada.Numerics.Generic_Elementary_Functions
     (Gdouble);
   use Functions;

   ---------------------
   -- Linearize_Angle --
   ---------------------

   function Linearize_Angle
     (I, N, a, k : Gdouble; Is_Derived : Boolean := False) return Gdouble
   is

      m : constant Gdouble := k * (1.0 - a) / (N - 1.0);
      p : constant Gdouble := k * (N * a - 1.0) / (N - 1.0);

   begin

      if not Is_Derived then
         return m * I + p;
      else
         return m;
      end if;

   end Linearize_Angle;

   ---------------
   -- Ln_Smooth --
   ---------------

   function Ln_Smooth (x : Gdouble) return Gdouble is

      x_t : constant Gdouble := x + 12.0;
      s   : constant Gdouble := 0.5;
      f_0 : constant Gdouble := (s * 12.0) / Log (e_d, 12.0);
   begin

      return (s * x_t) / (Log (e_d, x_t)) + (1.0 - f_0);

   end Ln_Smooth;

   ---------------------
   -- Linearize_Scale --
   ---------------------

   function Linearize_Scale (x, N : Gdouble) return Gdouble is
   begin
      return (1.0 / (N - 1.0)) * (N - 0.5 * (x + 1.0));
   end Linearize_Scale;

   -----------
   -- theta --
   -----------

   function theta (I, N, a, k, Start_Angle : Gdouble) return Gdouble is

      m : constant Gdouble := 0.05 * Sqrt (N);

   begin
      return TWO_PI * (1.0 - 2.0 * m * (I + 1.0) / N);
   end theta;

   ------------
   -- radius --
   ------------

   function radius (theta_var, N : Gdouble) return Gdouble is
      k : constant Gdouble := N / 10.0 + 0.5;
   begin
      return k * Phi**(2.0 * theta_var / PI);
   end radius;

   -----------------
   -- theta_prime --
   -----------------

   function theta_prime (I, N, a, k : Gdouble) return Gdouble is

   begin
      return -Ln_Smooth (I) * Linearize_Angle (I, N, a, k, Is_Derived => True);

   end theta_prime;

   ------------------
   -- radius_prime --
   ------------------

   function radius_prime (I, N, a, k, Start_Angle : Gdouble) return Gdouble is

      theta_var : constant Gdouble := theta (I, N, a, k, Start_Angle);
   begin

      return
        (2.0 * Log (e_d, Phi) / PI) * (theta_prime (I, N, a, k)) *
        radius (theta_var, N);
   end radius_prime;

   ------------------------
   -- Calculate_Gradient --
   ------------------------

   function Calculate_Gradient
     (I, N, a, k, Start_Angle : Gdouble; Is_Vowel : Boolean := False)
      return gradient
   is

      Gradient_Point : gradient;

      epsilon : constant Gdouble := 0.0;

      grad_r     : constant Gdouble := radius_prime (I, N, a, k, Start_Angle);
      grad_theta : constant Gdouble := theta_prime (I, N, a, k);

      grad_x : constant Gdouble := grad_r * Cos (grad_theta);
      grad_y : constant Gdouble := grad_r * Sin (grad_theta);

      Spiral_Side : constant Gdouble := (if Is_Vowel then 1.0 else -1.0);

   begin

      Ada.Text_IO.Put_Line
        (Gdouble'Image (grad_r) & ", " & Gdouble'Image (grad_theta));

      Gradient_Point.dx := Spiral_Side * epsilon * grad_x;
      Gradient_Point.dy := Spiral_Side * epsilon * grad_y;

      return Gradient_Point;

   end Calculate_Gradient;

end Math;
