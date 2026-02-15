--  Vision.Transformations -- Implementation

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

package body Vision.Transformations is

   package EF renames Ada.Numerics.Elementary_Functions;

   ----------------------------------------------------------
   --  ECI_To_Lat_Lon
   ----------------------------------------------------------
   --  Conversion simplifiee ECI -> geodesique (sans altitude).
   --  X, Y, Z en km dans le repere ECI.
   --  Lat = arctan(Z / sqrt(X^2 + Y^2))
   --  Lon = arctan2(Y, X)

   procedure ECI_To_Lat_Lon
     (Pos : Common.Physics.Position_Vector;
      Lat : out Long_Float;
      Lon : out Long_Float)
   is
      X : constant Long_Float :=
        Long_Float (Pos.X);
      Y : constant Long_Float :=
        Long_Float (Pos.Y);
      Z : constant Long_Float :=
        Long_Float (Pos.Z);

      R_XY : constant Float :=
        EF.Sqrt (Float (X * X + Y * Y));
   begin
      if R_XY < 1.0E-10 then
         --  Position au pole
         if Z >= 0.0 then
            Lat := Ada.Numerics.Pi / 2.0;
         else
            Lat := -(Ada.Numerics.Pi / 2.0);
         end if;
         Lon := 0.0;
      else
         Lat := Long_Float
           (EF.Arctan
              (Float (Z), R_XY));
         Lon := Long_Float
           (EF.Arctan
              (Float (Y), Float (X)));
      end if;
   end ECI_To_Lat_Lon;

   ----------------------------------------------------------
   --  Lat_Lon_To_Pixel
   ----------------------------------------------------------
   --  Projection equirectangulaire :
   --  Col = W * (lon + pi) / (2*pi)
   --  Row = H * (pi/2 - lat) / pi

   procedure Lat_Lon_To_Pixel
     (Lat : Long_Float;
      Lon : Long_Float;
      W   : Positive;
      H   : Positive;
      Row : out Positive;
      Col : out Positive)
   is
      use Ada.Numerics;

      Raw_Col : Integer :=
        Integer (Long_Float (W) * (Lon + Pi) / (2.0 * Pi));
      Raw_Row : Integer :=
        Integer (Long_Float (H) * (Pi / 2.0 - Lat) / Pi);
   begin
      --  Clamper dans les bornes
      if Raw_Col < 1 then
         Raw_Col := 1;
      elsif Raw_Col > W then
         Raw_Col := W;
      end if;

      if Raw_Row < 1 then
         Raw_Row := 1;
      elsif Raw_Row > H then
         Raw_Row := H;
      end if;

      Col := Raw_Col;
      Row := Raw_Row;
   end Lat_Lon_To_Pixel;

   ----------------------------------------------------------
   --  Satellite_Ground_Track
   ----------------------------------------------------------

   procedure Satellite_Ground_Track
     (Pos : Common.Physics.Position_Vector;
      Lat : out Long_Float;
      Lon : out Long_Float)
   is
   begin
      ECI_To_Lat_Lon (Pos, Lat, Lon);
   end Satellite_Ground_Track;

end Vision.Transformations;
