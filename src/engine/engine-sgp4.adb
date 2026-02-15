--  Engine.SGP4 -- Implementation du propagateur SGP4

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with Common.Logging;

package body Engine.SGP4 is

   package EF renames Ada.Numerics.Elementary_Functions;

   --  Constantes SGP4 (unites Earth Radii / minutes)
   KE   : constant Long_Float := 0.074366916133173;
   J2_C : constant Long_Float := 1.082_63E-03;
   --  Rayon terrestre en km
   RE   : constant Long_Float :=
     Long_Float (Common.Physics.Earth_Radius_Km);
   --  Minutes par jour
   Min_Per_Day : constant Long_Float := 1_440.0;
   --  2 * Pi
   Two_Pi : constant Long_Float :=
     2.0 * Ada.Numerics.Pi;

   --  x^(2/3) via exp/log
   function Pow_2_3 (X : Long_Float) return Long_Float
   is
      FX : constant Float := Float (X);
   begin
      return Long_Float
        (EF.Exp (2.0 / 3.0 * EF.Log (FX)));
   end Pow_2_3;

   ----------------------------------------------------------
   --  Minutes_Since_Epoch
   ----------------------------------------------------------

   function Minutes_Since_Epoch
     (State : SGP4_State;
      T     : Ada.Calendar.Time) return Long_Float
   is
      use type Ada.Calendar.Time;
      Diff : constant Duration :=
        T - State.TLE_Data.Epoch;
   begin
      return Long_Float (Diff) / 60.0;
   end Minutes_Since_Epoch;

   ----------------------------------------------------------
   --  Initialize
   ----------------------------------------------------------

   procedure Initialize
     (State : out SGP4_State;
      TLE   : Engine.TLE_Parser.TLE_Record)
   is
      use Common.Physics;

      Nn     : Long_Float;
      A1     : Long_Float;
      Cos_I0 : Float;
      Delta1 : Long_Float;
      A0dp   : Long_Float;
      P0     : Long_Float;
      Theta2 : Long_Float;
      Xi     : Long_Float;
   begin
      State.TLE_Data := TLE;

      State.E0     := Long_Float (TLE.Elements.Eccentricity);
      State.I0     := Long_Float (TLE.Elements.Inclination);
      State.Omega0 := Long_Float (TLE.Elements.RAAN);
      State.W0     := Long_Float (TLE.Elements.Arg_Of_Perigee);
      State.M0     := Long_Float (TLE.Elements.Mean_Anomaly);
      State.BStar  := TLE.BStar;

      --  rev/jour -> rad/min
      Nn := Long_Float (TLE.Elements.Mean_Motion)
        * Two_Pi / Min_Per_Day;
      State.N0 := Nn;

      --  a = (KE / n) ^ (2/3)
      A1 := Pow_2_3 (KE / Nn);

      --  Correction J2 de premier ordre
      Cos_I0 := EF.Cos (Float (State.I0));
      Theta2 := Long_Float (Cos_I0 * Cos_I0);

      Xi := 1.0
        / (A1 * (1.0 - State.E0 * State.E0));
      Delta1 :=
        0.75 * J2_C * Xi * Xi
        * (3.0 * Theta2 - 1.0);

      A0dp := A1
        * (1.0 - Delta1 / 3.0
           - Delta1 * Delta1
           - (134.0 / 81.0) * Delta1 ** 3);

      State.A0 := A0dp;

      P0 := A0dp * (1.0 - State.E0 * State.E0);

      --  d(RAAN)/dt
      State.Omega_Dot :=
        -1.5 * J2_C * (Nn / P0 ** 2)
        * Long_Float (Cos_I0);

      --  d(omega)/dt
      State.W_Dot :=
        0.75 * J2_C * (Nn / P0 ** 2)
        * (5.0 * Theta2 - 1.0);

      --  d(n)/dt (drag seculaire)
      State.N_Dot := TLE.Mean_Motion_Dot
        * Two_Pi / (Min_Per_Day * Min_Per_Day);

      State.Initialized := True;

      Common.Logging.Log_Info
        ("SGP4",
         "Initialise pour NORAD"
         & TLE.Catalog_Id'Image
         & " | a0="
         & Long_Float'Image (A0dp)
         & " ER");
   end Initialize;

   ----------------------------------------------------------
   --  Propagate
   ----------------------------------------------------------
   --  Calcule la position ECI a l'instant T.
   --
   --  Etapes :
   --  1. dt = minutes depuis l'epoque
   --  2. Mise a jour seculaire de n, omega, RAAN, M
   --  3. Equation de Kepler : M -> E (Newton-Raphson)
   --  4. Anomalie vraie + rayon
   --  5. Rotation plan orbital -> ECI

   function Propagate
     (State : SGP4_State;
      T     : Ada.Calendar.Time)
      return Common.Physics.State_Vector
   is
      use Common.Physics;

      Dt      : Long_Float;
      N_T     : Long_Float;
      E_T     : Long_Float;
      Omega_T : Long_Float;
      W_T     : Long_Float;
      M_T     : Long_Float;

      E_Anom   : Long_Float;
      Diff     : Long_Float;
      Max_Iter : constant := 10;

      Sin_E   : Float;
      Cos_E   : Float;
      Nu      : Long_Float;
      R_Mag   : Long_Float;

      R_Orb_X : Long_Float;
      R_Orb_Y : Long_Float;

      Sin_Om : Float;
      Cos_Om : Float;
      Sin_W  : Float;
      Cos_W  : Float;
      Sin_I  : Float;
      Cos_I  : Float;

      Result : State_Vector;
   begin
      Dt := Minutes_Since_Epoch (State, T);

      --  1. Mise a jour seculaire
      N_T := State.N0 + State.N_Dot * Dt;
      E_T := State.E0;
      Omega_T := State.Omega0 + State.Omega_Dot * Dt;
      W_T := State.W0 + State.W_Dot * Dt;
      M_T := State.M0 + N_T * Dt;

      --  Normaliser M dans [0, 2*pi]
      M_T := M_T - Two_Pi
        * Long_Float'Floor (M_T / Two_Pi);

      --  2. Equation de Kepler (Newton-Raphson)
      --  f(E)  = E - e*sin(E) - M
      --  f'(E) = 1 - e*cos(E)
      E_Anom := M_T;
      for Iter in 1 .. Max_Iter loop
         Diff := E_Anom
           - E_T * Long_Float
               (EF.Sin (Float (E_Anom)))
           - M_T;
         exit when abs (Diff) < 1.0E-12;
         E_Anom := E_Anom
           - Diff
           / (1.0 - E_T * Long_Float
               (EF.Cos (Float (E_Anom))));
      end loop;

      --  3. Anomalie vraie et rayon
      Sin_E := EF.Sin (Float (E_Anom));  --  reserve pour vitesse
      Cos_E := EF.Cos (Float (E_Anom));
      pragma Unreferenced (Sin_E);

      Nu := 2.0 * Long_Float
        (EF.Arctan
           (EF.Sqrt
              (Float ((1.0 + E_T) / (1.0 - E_T)))
            * EF.Tan (Float (E_Anom) / 2.0)));

      R_Mag := State.A0
        * (1.0 - E_T * Long_Float (Cos_E));

      --  4. Coordonnees plan orbital
      R_Orb_X := R_Mag
        * Long_Float (EF.Cos (Float (Nu)));
      R_Orb_Y := R_Mag
        * Long_Float (EF.Sin (Float (Nu)));

      --  5. Rotation vers ECI
      Sin_Om := EF.Sin (Float (Omega_T));
      Cos_Om := EF.Cos (Float (Omega_T));
      Sin_W  := EF.Sin (Float (W_T));
      Cos_W  := EF.Cos (Float (W_T));
      Sin_I  := EF.Sin (Float (State.I0));
      Cos_I  := EF.Cos (Float (State.I0));

      declare
         Ux : constant Long_Float := Long_Float
           (Cos_Om * Cos_W - Sin_Om * Sin_W * Cos_I);
         Uy : constant Long_Float := Long_Float
           (Sin_Om * Cos_W + Cos_Om * Sin_W * Cos_I);
         Uz : constant Long_Float := Long_Float
           (Sin_W * Sin_I);

         Vx : constant Long_Float := Long_Float
           (-(Cos_Om * Sin_W
              + Sin_Om * Cos_W * Cos_I));
         Vy : constant Long_Float :=
           Long_Float (-(Sin_Om * Sin_W))
           + Long_Float (Cos_Om * Cos_W * Cos_I);
         Vz : constant Long_Float := Long_Float
           (Cos_W * Sin_I);

         X_Km : constant Long_Float :=
           (R_Orb_X * Ux + R_Orb_Y * Vx) * RE;
         Y_Km : constant Long_Float :=
           (R_Orb_X * Uy + R_Orb_Y * Vy) * RE;
         Z_Km : constant Long_Float :=
           (R_Orb_X * Uz + R_Orb_Y * Vz) * RE;
      begin
         Result.Position := Position_Vector'
           (X => Coordinate (X_Km),
            Y => Coordinate (Y_Km),
            Z => Coordinate (Z_Km));
      end;

      Result.Velocity := Velocity_Vector'
        (Xdot => 0.0, Ydot => 0.0, Zdot => 0.0);
      Result.Epoch := T;

      return Result;
   end Propagate;

end Engine.SGP4;
