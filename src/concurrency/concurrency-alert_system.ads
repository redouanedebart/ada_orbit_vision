--  Concurrency.Alert_System -- Surveillance de collisions
--
--  Tache haute priorite qui interroge periodiquement le
--  Ground_Station pour verifier la distance entre chaque
--  paire de satellites. Declenche une alerte si la distance
--  descend sous le seuil.

with Common.Physics;

package Concurrency.Alert_System is

   --  Intervalle de verification par defaut
   Default_Check_Interval : constant Duration := 2.0;

   --  Tache de surveillance
   task type Alert_Monitor
     (Check_Interval : access Duration;
      Threshold_Km   : access Common.Physics.Coordinate)
   is
      entry Start;
      entry Stop;
   end Alert_Monitor;

   type Monitor_Access is access Alert_Monitor;

end Concurrency.Alert_System;
