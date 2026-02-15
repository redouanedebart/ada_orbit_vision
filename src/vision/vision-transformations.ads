--  Vision.Transformations -- Projections et conversions de coordonnees
--
--  Convertit les positions ECI en coordonnees geographiques
--  (latitude/longitude) et mappe ces coordonnees vers des
--  pixels dans une projection equirectangulaire.

with Common.Physics;

package Vision.Transformations is

   --  Convertit une position ECI en latitude/longitude.
   --  Lat en radians [-pi/2, pi/2], Lon en radians [-pi, pi].
   --  Approximation : ne tient pas compte de l'altitude.
   procedure ECI_To_Lat_Lon
     (Pos : Common.Physics.Position_Vector;
      Lat : out Long_Float;
      Lon : out Long_Float);

   --  Mappe une latitude/longitude vers des coordonnees pixel
   --  dans une image equirectangulaire de taille W x H.
   procedure Lat_Lon_To_Pixel
     (Lat : Long_Float;
      Lon : Long_Float;
      W   : Positive;
      H   : Positive;
      Row : out Positive;
      Col : out Positive);

   --  Wrapper : position ECI -> (Lat, Lon) directement
   procedure Satellite_Ground_Track
     (Pos : Common.Physics.Position_Vector;
      Lat : out Long_Float;
      Lon : out Long_Float);

end Vision.Transformations;
