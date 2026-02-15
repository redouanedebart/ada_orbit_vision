--  Common.Physics — Implémentation des sous-programmes utilitaires

with Ada.Numerics.Elementary_Functions;

package body Common.Physics is

   package EF renames Ada.Numerics.Elementary_Functions;

   ---------------------------------------------------------------------------
   --  Distance euclidienne entre deux positions ECI
   ---------------------------------------------------------------------------
   --  Stratégie : les opérations sur types fixed-point ne supportent pas
   --  Sqrt directement. On convertit chaque différence en Float, on
   --  calcule en flottant, puis on reconvertit en Coordinate.

   function Distance
     (A, B : Position_Vector) return Coordinate
   is
      Dx : constant Float := Float (A.X) - Float (B.X);
      Dy : constant Float := Float (A.Y) - Float (B.Y);
      Dz : constant Float := Float (A.Z) - Float (B.Z);
   begin
      return Coordinate (EF.Sqrt (Dx * Dx + Dy * Dy + Dz * Dz));
   end Distance;

   ---------------------------------------------------------------------------
   --  Norme (magnitude) d'un vecteur position
   ---------------------------------------------------------------------------

   function Norm (V : Position_Vector) return Coordinate
   is
      Fx : constant Float := Float (V.X);
      Fy : constant Float := Float (V.Y);
      Fz : constant Float := Float (V.Z);
   begin
      return Coordinate (EF.Sqrt (Fx * Fx + Fy * Fy + Fz * Fz));
   end Norm;

end Common.Physics;
