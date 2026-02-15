--  Engine.Collision -- Implementation de la detection de collision

with Common.Logging;

package body Engine.Collision is

   ----------------------------------------------------------
   --  Evaluate
   ----------------------------------------------------------

   function Evaluate
     (A, B      : Common.Physics.Position_Vector;
      Threshold : Common.Physics.Coordinate :=
        Common.Physics.Default_Collision_Threshold_Km)
      return Proximity_Result
   is
      use Common.Physics;
      D : constant Coordinate := Distance (A, B);
   begin
      if D < Threshold then
         Common.Logging.Log_Warning
           ("Collision",
            "Proximite detectee :"
            & Coordinate'Image (D) & " km"
            & " (seuil ="
            & Coordinate'Image (Threshold)
            & " km)");
      end if;

      return Proximity_Result'
        (Dist      => D,
         Is_Alert  => D < Threshold,
         Threshold => Threshold);
   end Evaluate;

   ----------------------------------------------------------
   --  Is_Collision
   ----------------------------------------------------------
   function Is_Collision
     (A, B      : Common.Physics.Position_Vector;
      Threshold : Common.Physics.Coordinate :=
        Common.Physics.Default_Collision_Threshold_Km)
      return Boolean
   is
   begin
      return Evaluate (A, B, Threshold).Is_Alert;
   end Is_Collision;

end Engine.Collision;
