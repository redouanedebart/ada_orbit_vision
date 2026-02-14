--  Common.Physics — Types fondamentaux et constantes physiques
--
--  Ce paquetage définit les types à virgule fixe pour les coordonnées
--  orbitales, les vecteurs d'état (position + vitesse), ainsi que les
--  constantes physiques nécessaires au propagateur SGP4.
with Ada.Calendar;
with Ada.Numerics;

package Common.Physics is

   ---------------------------------------------------------------------------
   --  Types à virgule fixe — précision sans dérive flottante
   ---------------------------------------------------------------------------

   --  Coordonnée spatiale en kilomètres.
   --  Résolution : 1 micromètre (10^-6 km = 1 mm).
   --  Plage : couvre largement l'orbite géostationnaire (~42 164 km).
   type Coordinate is
      delta 10.0**(-6) range -20_000_000.0 .. 20_000_000.0;

   --  Composante de vitesse orbitale en km/s.
   --  Résolution : 10^-9 (nanomètre/s).
   type Velocity is
      delta 10.0**(-9) range -50_000.0 .. 50_000.0;

   --  Angle orbital en radians. Plage couvrant -2π .. 2π.
   type Angle is
      delta 10.0**(-12) range -6.3 .. 6.3;

   ---------------------------------------------------------------------------
   --  Vecteurs d'état
   ---------------------------------------------------------------------------

   --  Vecteur position 3D (X, Y, Z) en repère ECI (Earth-Centered Inertial)
   type Position_Vector is record
      X : Coordinate := 0.0;
      Y : Coordinate := 0.0;
      Z : Coordinate := 0.0;
   end record;

   --  Vecteur vitesse 3D (Xdot, Ydot, Zdot) en km/s, repère ECI
   type Velocity_Vector is record
      Xdot : Velocity := 0.0;
      Ydot : Velocity := 0.0;
      Zdot : Velocity := 0.0;
   end record;

   --  Vecteur d'état complet d'un satellite à un instant donné
   type State_Vector is record
      Position : Position_Vector;
      Velocity : Velocity_Vector;
      Epoch    : Ada.Calendar.Time;
   end record;
   ---------------------------------------------------------------------------
   --  Éléments képlériens (sortie du parseur TLE)
   ---------------------------------------------------------------------------

   --  Éléments orbitaux classiques issus d'un TLE NORAD
   type Keplerian_Elements is record
      Inclination    : Angle;
      RAAN           : Angle;
      Eccentricity   : Coordinate;  -- 0.0 .. 1.0
      Arg_Of_Perigee : Angle;
      Mean_Anomaly   : Angle;
      Mean_Motion    : Coordinate;  -- à convertir en rad/s
   end record;
   ---------------------------------------------------------------------------
   --  Constantes physiques (système SI, distances en km)
   ---------------------------------------------------------------------------

   --  Rayon moyen équatorial terrestre (WGS-84)
   Earth_Radius_Km : constant := 6_378.137;

   --  Paramètre gravitationnel standard (GM) de la Terre
   --  Unité : km³/s²
   GM_Earth : constant := 398_600.4418;

   --  Coefficient J2 d'aplatissement terrestre (harmonique zonale degré 2)
   J2 : constant := 1.082_63 * 10.0**(-3);

   --  Vitesse de rotation terrestre (rad/s) — jour sidéral = 86 164.1 s
   Earth_Rotation_Rate : constant :=
      2.0 * Ada.Numerics.Pi / 86_164.1;


   --  Seuil de collision par défaut (km)
   --  Deux objets plus proches que cette distance déclenchent une alerte.
   Default_Collision_Threshold_Km : constant := 10.0;

   ---------------------------------------------------------------------------
   --  Sous-programmes utilitaires
   ---------------------------------------------------------------------------

   --  Distance euclidienne entre deux positions dans l'espace ECI
   --  TODO : Implémenter dans common-physics.adb
   --  Formule : sqrt((Ax-Bx)² + (Ay-By)² + (Az-Bz)²)
   --  Attention : les types fixed-point ne supportent pas directement
   --  sqrt — il faudra convertir en Float pour le calcul.
   function Distance
     (A, B : Position_Vector) return Coordinate;

   --  Norme (magnitude) d'un vecteur position
   --  TODO : Implémenter dans common-physics.adb
   function Norm (V : Position_Vector) return Coordinate;

end Common.Physics;
