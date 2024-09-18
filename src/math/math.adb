with Ada.Numerics.Generic_Elementary_Functions;

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
      return N / 10.0 + 0.5;
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

   ------------------------
   -- Calculate_Gradient --
   ------------------------

   function Calculate_Gradient
     (I, N : Gdouble; Is_Vowel : Boolean := False) return gradient
   is

      Gradient_Point : gradient;

      epsilon : constant Gdouble := 0.1;

      grad_r     : constant Gdouble := radius_prime (I, N);
      grad_theta : constant Gdouble := theta_prime (N);

      grad_x : constant Gdouble := grad_r * Cos (grad_theta);
      grad_y : constant Gdouble := grad_r * Sin (grad_theta);

      Spiral_Side : constant Gdouble := (if Is_Vowel then -1.0 else 1.0);

   begin

      Gradient_Point.dx := Spiral_Side * epsilon * grad_x;
      Gradient_Point.dy := Spiral_Side * epsilon * grad_y;

      return Gradient_Point;

   end Calculate_Gradient;

end Math;
