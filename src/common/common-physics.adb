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
   --  TODO : Implémenter cette fonction.
   --  Formule : sqrt(X² + Y² + Z²)
   --  Indice : même approche que Distance — convertir en Float,
   --  calculer Sqrt via EF.Sqrt, reconvertir en Coordinate.
   --  Bonus : réutiliser Distance avec un vecteur origine (0, 0, 0)
   --  serait élégant mais moins performant (allocation d'un record).

   function Norm (V : Position_Vector) return Coordinate is
   begin
      --  TODO : remplacer ce « raise » par le vrai calcul
      raise Program_Error with "Norm : pas encore implémenté";
      return 0.0;
   end Norm;

end Common.Physics;
