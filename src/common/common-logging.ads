--  Common.Logging — Système de journalisation centralisé et task-safe
--
--  Utilise un objet protégé interne pour garantir que tous les appels
--  depuis n'importe quelle tâche Ada sont sérialisés sans verrou manuel.
--  Les messages sont horodatés et écrits dans un fichier et/ou sur
--  Standard_Error.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Common.Logging is

   --  Niveaux de sévérité, du moins au plus critique
   type Log_Level is (Debug, Info, Warning, Error);

   --  Initialise le logger.
   --  File_Path    : chemin du fichier de log (créé si absent).
   --  Console      : si True, duplique aussi sur Standard_Error.
   --  Min_Level    : seuil minimal affiché (messages en dessous ignorés).
   procedure Initialize
     (File_Path : String    := "logs/ada_orbit_vision.log";
      Console   : Boolean   := True;
      Min_Level : Log_Level := Info);

   --  Point d'entrée principal pour émettre un message.
   --  Source identifie le module appelant (ex. "SGP4", "Alert_System").
   procedure Log
     (Level   : Log_Level;
      Source  : String;
      Message : String);

   --  Raccourcis par niveau
   procedure Log_Debug   (Source : String; Message : String);
   procedure Log_Info    (Source : String; Message : String);
   procedure Log_Warning (Source : String; Message : String);
   procedure Log_Error   (Source : String; Message : String);

   --  Ferme proprement le fichier de log.
   procedure Finalize;

private

   --  Objet protégé assurant l'accès exclusif au fichier de log.
   --  Toutes les écritures passent par Write_Entry.
   protected Logger is
      procedure Set_Config
        (File_Path : String;
         Console   : Boolean;
         Min_Level : Log_Level);

      procedure Write_Entry
        (Level   : Log_Level;
         Source  : String;
         Message : String);

      procedure Close;
   private
      Configured    : Boolean        := False;
      Log_To_Stderr : Boolean        := True;
      Minimum_Level : Log_Level      := Info;
      Log_File_Path : Unbounded_String := Null_Unbounded_String;
      File_Open     : Boolean        := False;
   end Logger;

end Common.Logging;
