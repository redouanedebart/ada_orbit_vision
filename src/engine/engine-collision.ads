--  Engine.Collision -- Detection de collision entre satellites
--
--  Compare les positions de paires de satellites et determine
--  si leur distance euclidienne dans le repere ECI descend
--  sous un seuil configurable.

with Common.Physics;

package Engine.Collision is

   --  Resultat d'une evaluation de proximite entre deux objets
   type Proximity_Result is record
      Dist       : Common.Physics.Coordinate;
      Is_Alert   : Boolean;
      Threshold  : Common.Physics.Coordinate;
   end record;

   --  Evalue la proximite entre deux positions.
   --  Retourne la distance et un booleen d'alerte si
   --  la distance < seuil.
   function Evaluate
     (A, B      : Common.Physics.Position_Vector;
      Threshold : Common.Physics.Coordinate :=
        Common.Physics.Default_Collision_Threshold_Km)
      return Proximity_Result;

   --  Raccourci : retourne True si distance < seuil
   function Is_Collision
     (A, B      : Common.Physics.Position_Vector;
      Threshold : Common.Physics.Coordinate :=
        Common.Physics.Default_Collision_Threshold_Km)
      return Boolean;

end Engine.Collision;
