--  Engine.SGP4 -- Propagateur orbital simplifie SGP4
--
--  Implemente le modele SGP4 (Simplified General Perturbations 4)
--  pour predire la position et la vitesse d'un satellite a un
--  instant donne, a partir de ses elements orbitaux TLE.
--
--  Reference : Vallado, Crawford, Hujsak, Kelso (2006)
--  "Revisiting Spacetrack Report #3"

with Ada.Calendar;
with Common.Physics;
with Engine.TLE_Parser;

package Engine.SGP4 is

   --  Donnees SGP4 pre-calculees a partir d'un TLE.
   --  Initialiser une fois via Initialize, puis appeler
   --  Propagate autant de fois que necessaire.
   type SGP4_State is private;

   --  Initialise l'etat SGP4 a partir d'un TLE parse.
   --  Pre-calcule toutes les constantes derivees
   --  (demi-grand axe, mouvement moyen corrige, etc.)
   procedure Initialize
     (State : out SGP4_State;
      TLE   : Engine.TLE_Parser.TLE_Record);

   --  Propage la position du satellite a l'instant T.
   --  Retourne le vecteur d'etat (position + vitesse)
   --  dans le repere ECI.
   function Propagate
     (State : SGP4_State;
      T     : Ada.Calendar.Time)
      return Common.Physics.State_Vector;

   --  Calcule le temps ecoule en minutes entre l'epoque
   --  TLE et un instant donne.
   function Minutes_Since_Epoch
     (State : SGP4_State;
      T     : Ada.Calendar.Time) return Long_Float;

private

   --  Constantes pre-calculees pour l'orbite
   type SGP4_State is record
      --  Elements d'origine
      TLE_Data     : Engine.TLE_Parser.TLE_Record;

      --  Constantes derivees (calculees dans Initialize)
      N0           : Long_Float := 0.0;  --  mouvement moyen rad/min
      A0           : Long_Float := 0.0;  --  demi-grand axe (ER)
      E0           : Long_Float := 0.0;  --  excentricite
      I0           : Long_Float := 0.0;  --  inclinaison (rad)
      Omega0       : Long_Float := 0.0;  --  RAAN (rad)
      W0           : Long_Float := 0.0;  --  arg perigee (rad)
      M0           : Long_Float := 0.0;  --  anomalie moyenne (rad)
      BStar        : Long_Float := 0.0;  --  coeff trainee

      --  Corrections seculaires J2
      N_Dot        : Long_Float := 0.0;  --  d(n)/dt
      Omega_Dot    : Long_Float := 0.0;  --  d(RAAN)/dt
      W_Dot        : Long_Float := 0.0;  --  d(omega)/dt

      Initialized  : Boolean := False;
   end record;

end Engine.SGP4;
