--  Engine.Footprint -- Calcul de l'empreinte au sol d'un satellite
--
--  Donne la position au sol d'un satellite (lat/lon) et son
--  altitude, calcule le cone de visibilite (half-angle) et le
--  polygone au sol (spherical cap) en coordonnees lat/lon.

package Engine.Footprint is

   --  Point geographique (latitude et longitude en radians)
   type Geo_Point is record
      Lat : Long_Float := 0.0;
      Lon : Long_Float := 0.0;
   end record;

   --  Nombre de sommets du polygone footprint
   Max_Points : constant Positive := 72;

   --  Tableau de points du contour footprint
   type Footprint_Array is
     array (Positive range 1 .. Max_Points) of Geo_Point;

   --  Calcule le demi-angle de vision (radians) depuis l'altitude.
   --  half_angle = arccos(R_Terre / (R_Terre + altitude))
   --  Retourne 0.0 si altitude <= 0.
   function Half_Angle_Rad
     (Altitude_Km : Long_Float) return Long_Float;

   --  Calcule le polygone du footprint en Max_Points points.
   --  Center_Lat, Center_Lon : latitude/longitude du nadir (radians).
   --  Radius_Rad : demi-angle du cone de visibilite (radians).
   --  Points : tableau de sortie des sommets lat/lon (radians).
   --
   --  Formule trigonometrique spherique :
   --    lat_p = arcsin(sin(lat0)*cos(R) + cos(lat0)*sin(R)*cos(theta))
   --    dlon  = arctan2(sin(theta)*sin(R)*cos(lat0),
   --                    cos(R) - sin(lat0)*sin(lat_p))
   --    lon_p = lon0 + dlon
   procedure Compute_Polygon
     (Center_Lat : Long_Float;
      Center_Lon : Long_Float;
      Radius_Rad : Long_Float;
      Points     : out Footprint_Array);

end Engine.Footprint;
