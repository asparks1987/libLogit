--  Ada binding for the libLogit JSON configuration.
with Ada.Strings.Unbounded;

package LibLogit is
   type Log_Level is (Trace, Debug, Info, Warn, Error, Fatal);

   type Config is record
      Threshold : Log_Level := Info;
      Tag_Level : Boolean := True;
      Timestamp : Boolean := True;
      File_Location : Ada.Strings.Unbounded.Unbounded_String;
      Network_File_Location : Ada.Strings.Unbounded.Unbounded_String;
      Has_File : Boolean := False;
      Has_Network : Boolean := False;
   end record;

   --  Load configuration data from the JSON file located at Path.
   procedure Initialize (Path : String);
   --  Emit Message at the requested Level using configured sinks.
   procedure Log_Message (Level : Log_Level; Message : String);

private
   --  Internal configuration state lives in the body for now.
end LibLogit;
