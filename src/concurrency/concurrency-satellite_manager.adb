--  Concurrency.Satellite_Manager -- Implementation

with Ada.Calendar;
with Common.Logging;
with Common.Physics;
with Engine.SGP4;
with Concurrency.Ground_Station;

package body Concurrency.Satellite_Manager is

   ----------------------------------------------------------
   --  Satellite_Tracker -- Corps de la tache
   ----------------------------------------------------------
   --  Boucle principale :
   --  1. Recevoir le TLE via l'entry Start
   --  2. Initialiser l'etat SGP4
   --  3. Boucle de propagation :
   --     a. Propager a Ada.Calendar.Clock
   --     b. Construire un Snapshot
   --     c. Deposer dans Ground_Station
   --     d. Attendre Refresh secondes ou entry Stop

   task body Satellite_Tracker is
      SGP4  : Engine.SGP4.SGP4_State;
      TLE_R : Engine.TLE_Parser.TLE_Record;
      Running : Boolean := False;
   begin
      --  Attente du TLE initial
      accept Start (TLE : Engine.TLE_Parser.TLE_Record)
      do
         TLE_R := TLE;
      end Start;

      --  Initialisation SGP4
      Engine.SGP4.Initialize (SGP4, TLE_R);
      Running := True;

      Common.Logging.Log_Info
        ("Tracker",
         "Demarrage suivi NORAD"
         & TLE_R.Catalog_Id'Image);

      --  Boucle de propagation
      while Running loop
         declare
            use Common.Physics;

            Now : constant Ada.Calendar.Time :=
              Ada.Calendar.Clock;
            SV  : constant State_Vector :=
              Engine.SGP4.Propagate (SGP4, Now);

            Snap : Ground_Station.Satellite_Snapshot;
         begin
            Snap.Id       := TLE_R.Catalog_Id;
            Snap.Name     := TLE_R.Name;
            Snap.Name_Len := TLE_R.Name_Length;
            Snap.State    := SV;

            Ground_Station.Station.Update_Position
              (Snap);
         end;

         --  Attente avec ecoute de Stop
         select
            accept Stop do
               Running := False;
            end Stop;
         or
            delay Refresh.all;
         end select;
      end loop;

      Common.Logging.Log_Info
        ("Tracker",
         "Arret suivi NORAD"
         & TLE_R.Catalog_Id'Image);
   end Satellite_Tracker;

end Concurrency.Satellite_Manager;
