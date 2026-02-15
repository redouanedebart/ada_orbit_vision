--  Concurrency.Satellite_Manager -- Taches de suivi orbital
--
--  Chaque satellite est suivi par une instance independante
--  de Satellite_Tracker. La tache execute une boucle :
--    1. Propagate SGP4 a l'instant courant
--    2. Deposer le resultat dans Ground_Station
--    3. Attendre l'intervalle de rafraichissement
--    4. Repeter

with Engine.TLE_Parser;

package Concurrency.Satellite_Manager is

   --  Intervalle de rafraichissement par defaut (secondes)
   Default_Refresh_Interval : constant Duration := 1.0;

   --  Type tache : une instance par satellite
   task type Satellite_Tracker
     (Refresh : access Duration)
   is
      --  Demarre le suivi avec un TLE donne
      entry Start (TLE : Engine.TLE_Parser.TLE_Record);

      --  Demande l'arret propre de la tache
      entry Stop;
   end Satellite_Tracker;

   --  Acces pour allocation dynamique
   type Tracker_Access is access Satellite_Tracker;

end Concurrency.Satellite_Manager;
