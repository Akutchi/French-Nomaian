with Ada.Numerics.Generic_Elementary_Functions;

with Math_Constants; use Math_Constants;

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

   function Ln_Smooth
     (x : Gdouble; Is_Derived : Boolean := False) return Gdouble
   is

      x_t : constant Gdouble := x + 12.0;
      s   : constant Gdouble := 0.005;
      f_0 : constant Gdouble := (s * 12.0) / Log (e_d, 12.0);
   begin

      if not Is_Derived then
         return (s * x_t) / (Log (e_d, x_t)) + (1.0 - f_0);
      else
         return s * (Log (e_d, x_t) - 1.0) / (Log (e_d, x_t)**2);
      end if;

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

   function theta (I, N, Start_Angle, a, k : Gdouble) return Gdouble is
   begin
      return Start_Angle - Ln_Smooth (N) * Linearize_Angle (I, N, a, k);
   end theta;

   ------------
   -- radius --
   ------------

   function radius (theta_var : Gdouble) return Gdouble is
   begin
      return Phi**(2.0 * theta_var / PI);
   end radius;

   -----------------
   -- theta_prime --
   -----------------

   function theta_prime (I, N, a, k : Gdouble) return Gdouble is

      lt : constant Gdouble :=
        Ln_Smooth (I, Is_Derived => True) * Linearize_Angle (I, N, a, k);

      rt : constant Gdouble :=
        Ln_Smooth (I) * Linearize_Angle (I, N, a, k, Is_Derived => True);

   begin
      return -(lt + rt);

   end theta_prime;

   ------------------
   -- radius_prime --
   ------------------

   function radius_prime (I, N, Start_Angle, a, k : Gdouble) return Gdouble is
   begin
      return
        (2.0 * Log (e_d, Phi) / PI) * theta (I, N, Start_Angle, a, k) *
        theta_prime (I, N, a, k);
   end radius_prime;

end Math;
