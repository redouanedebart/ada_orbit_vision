--  Common.Logging — Implémentation du système de journalisation

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Common.Logging is

   ----------------------------------------------------------------
   --  Fonctions utilitaires internes
   ----------------------------------------------------------------

   function Level_Tag (Level : Log_Level) return String is
   begin
      case Level is
         when Debug   => return "DEBUG";
         when Info    => return "INFO ";
         when Warning => return "WARN ";
         when Error   => return "ERROR";
      end case;
   end Level_Tag;

   function Timestamp return String is
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      return Ada.Calendar.Formatting.Image (Now);
   end Timestamp;

   ----------------------------------------------------------------
   --  Corps de l'objet protégé Logger
   ----------------------------------------------------------------

   protected body Logger is

      procedure Set_Config
        (File_Path : String;
         Console   : Boolean;
         Min_Level : Log_Level)
      is
         Dir : constant String :=
           Ada.Directories.Containing_Directory (File_Path);
      begin
         --  Crée le répertoire parent si nécessaire
         if not Ada.Directories.Exists (Dir) then
            Ada.Directories.Create_Path (Dir);
         end if;

         Log_File_Path := To_Unbounded_String (File_Path);
         Log_To_Stderr := Console;
         Minimum_Level := Min_Level;
         Configured    := True;
      end Set_Config;

      procedure Write_Entry
        (Level   : Log_Level;
         Source  : String;
         Message : String)
      is
         File : Ada.Text_IO.File_Type;
         Line : constant String :=
           "[" & Timestamp & "] "
           & Level_Tag (Level) & " ["
           & Source & "] " & Message;
      begin
         if not Configured then
            return;
         end if;

         if Level < Minimum_Level then
            return;
         end if;

         --  Écriture dans le fichier (ouverture en mode Append)
         begin
            Ada.Text_IO.Open
              (File, Ada.Text_IO.Append_File,
               To_String (Log_File_Path));
         exception
            when Ada.Text_IO.Name_Error =>
               Ada.Text_IO.Create
                 (File, Ada.Text_IO.Out_File,
                  To_String (Log_File_Path));
         end;
         Ada.Text_IO.Put_Line (File, Line);
         Ada.Text_IO.Close (File);

         --  Duplication sur Standard_Error si demandé
         if Log_To_Stderr then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Line);
         end if;
      end Write_Entry;

      procedure Close is
      begin
         Configured := False;
         File_Open  := False;
      end Close;

   end Logger;

   ----------------------------------------------------------------
   --  Interface publique
   ----------------------------------------------------------------

   procedure Initialize
     (File_Path : String    := "logs/ada_orbit_vision.log";
      Console   : Boolean   := True;
      Min_Level : Log_Level := Info)
   is
   begin
      Logger.Set_Config (File_Path, Console, Min_Level);
      Logger.Write_Entry
        (Info, "Logging", "Logger initialise - niveau minimum : "
         & Log_Level'Image (Min_Level));
   end Initialize;

   procedure Log
     (Level   : Log_Level;
      Source  : String;
      Message : String)
   is
   begin
      Logger.Write_Entry (Level, Source, Message);
   end Log;

   procedure Log_Debug (Source : String; Message : String) is
   begin
      Logger.Write_Entry (Debug, Source, Message);
   end Log_Debug;

   procedure Log_Info (Source : String; Message : String) is
   begin
      Logger.Write_Entry (Info, Source, Message);
   end Log_Info;

   procedure Log_Warning (Source : String; Message : String) is
   begin
      Logger.Write_Entry (Warning, Source, Message);
   end Log_Warning;

   procedure Log_Error (Source : String; Message : String) is
   begin
      Logger.Write_Entry (Error, Source, Message);
   end Log_Error;

   procedure Finalize is
   begin
      Logger.Write_Entry
        (Info, "Logging", "Fermeture du logger");
      Logger.Close;
   end Finalize;

end Common.Logging;
