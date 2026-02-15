--  Concurrency.Alert_System -- Implementation

with Common.Logging;
with Engine.Collision;
with Concurrency.Ground_Station;

package body Concurrency.Alert_System is

   task body Alert_Monitor is
      Running : Boolean := False;
   begin
      accept Start;
      Running := True;

      Common.Logging.Log_Info
        ("Alert_System", "Surveillance demarree");

      while Running loop
         --  Lire toutes les positions
         declare
            use Concurrency.Ground_Station;
            use Snapshot_Maps;

            All_Sats : constant Snapshot_Maps.Map :=
              Station.Get_All;
            C1 : Cursor := All_Sats.First;
            C2 : Cursor;
         begin
            --  Comparer chaque paire (i, j) avec i < j
            while C1 /= No_Element loop
               C2 := Next (C1);
               while C2 /= No_Element loop
                  declare
                     S1 : constant Satellite_Snapshot :=
                       Element (C1);
                     S2 : constant Satellite_Snapshot :=
                       Element (C2);

                     Res : constant
                       Engine.Collision.Proximity_Result
                       :=
                       Engine.Collision.Evaluate
                         (S1.State.Position,
                          S2.State.Position,
                          Threshold_Km.all);
                  begin
                     if Res.Is_Alert then
                        Common.Logging.Log_Error
                          ("Alert_System",
                           "ALERTE COLLISION :"
                           & S1.Id'Image & " <->"
                           & S2.Id'Image
                           & " dist="
                           & Common.Physics
                               .Coordinate'Image
                               (Res.Dist)
                           & " km");
                     end if;
                  end;
                  C2 := Next (C2);
               end loop;
               C1 := Next (C1);
            end loop;
         end;

         --  Attente avec ecoute de Stop
         select
            accept Stop do
               Running := False;
            end Stop;
         or
            delay Check_Interval.all;
         end select;
      end loop;

      Common.Logging.Log_Info
        ("Alert_System", "Surveillance arretee");
   end Alert_Monitor;

end Concurrency.Alert_System;
