--  Ada_Orbit_Vision -- Point d'entree principal (stub)
with Common.Logging;
with Engine.TLE_Parser;
with Engine.Collision;
with Engine.SGP4;
with Concurrency.Ground_Station;
with Concurrency.Satellite_Manager;
with Concurrency.Alert_System;
pragma Unreferenced (Engine.TLE_Parser);
pragma Unreferenced (Engine.Collision);
pragma Unreferenced (Engine.SGP4);
pragma Unreferenced (Concurrency.Ground_Station);
pragma Unreferenced (Concurrency.Satellite_Manager);
pragma Unreferenced (Concurrency.Alert_System);

procedure Ada_Orbit_Vision is
begin
   Common.Logging.Initialize;
   Common.Logging.Log_Info
     ("Main", "Ada Orbit Vision demarre");
   Common.Logging.Finalize;
end Ada_Orbit_Vision;
