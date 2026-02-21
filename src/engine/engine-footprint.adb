--  Engine.Footprint -- Implementation

with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;

package body Engine.Footprint is

   use Ada.Numerics.Long_Elementary_Functions;

   Earth_R : constant Long_Float := 6_378.137;

   ----------------------------------------------------------
   --  Half_Angle_Rad
   ----------------------------------------------------------

   function Half_Angle_Rad
     (Altitude_Km : Long_Float) return Long_Float
   is
   begin
      if Altitude_Km <= 0.0 then
         return 0.0;
      end if;
      return Arccos (Earth_R / (Earth_R + Altitude_Km));
   end Half_Angle_Rad;

   ----------------------------------------------------------
   --  Compute_Polygon
   ----------------------------------------------------------

   procedure Compute_Polygon
     (Center_Lat : Long_Float;
      Center_Lon : Long_Float;
      Radius_Rad : Long_Float;
      Points     : out Footprint_Array)
   is
      Pi      : constant Long_Float := Ada.Numerics.Pi;
      Step    : constant Long_Float :=
        2.0 * Pi / Long_Float (Max_Points);
      Sin_R   : constant Long_Float := Sin (Radius_Rad);
      Cos_R   : constant Long_Float := Cos (Radius_Rad);
      Sin_Lat : constant Long_Float := Sin (Center_Lat);
      Cos_Lat : constant Long_Float := Cos (Center_Lat);
      Theta   : Long_Float;
      Lat_P   : Long_Float;
      Dlon    : Long_Float;
      Sin_P   : Long_Float;
      Num     : Long_Float;
      Den     : Long_Float;
   begin
      for I in 1 .. Max_Points loop
         Theta := Long_Float (I - 1) * Step;

         --  Sinus de la latitude du point du footprint
         --  (formule trigonometrique spherique)
         Sin_P :=
           Sin_Lat * Cos_R + Cos_Lat * Sin_R * Cos (Theta);

         --  Clamper pour eviter les erreurs numeriques
         if Sin_P > 1.0 then
            Sin_P := 1.0;
         elsif Sin_P < -1.0 then
            Sin_P := -1.0;
         end if;

         Lat_P := Arcsin (Sin_P);

         --  Difference de longitude via atan2
         Num := Sin (Theta) * Sin_R * Cos_Lat;
         Den := Cos_R - Sin_Lat * Sin_P;

         if abs (Num) < 1.0E-15 and then abs (Den) < 1.0E-15 then
            Dlon := 0.0;
         else
            Dlon := Arctan (Num, Den);
         end if;

         Points (I) :=
           (Lat => Lat_P,
            Lon => Center_Lon + Dlon);
      end loop;
   end Compute_Polygon;

end Engine.Footprint;
