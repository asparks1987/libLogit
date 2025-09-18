with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body LibLogit is

   Current_Config : Config;
   Configured     : Boolean := False;

   function Level_To_String (Level : Log_Level) return String is
   begin
      case Level is
         when Trace => return "TRACE";
         when Debug => return "DEBUG";
         when Info  => return "INFO";
         when Warn  => return "WARN";
         when Error => return "ERROR";
         when Fatal => return "FATAL";
      end case;
   end Level_To_String;

   function Level_Rank (Level : Log_Level) return Integer is
   begin
      case Level is
         when Trace => return 0;
         when Debug => return 1;
         when Info  => return 2;
         when Warn  => return 3;
         when Error => return 4;
         when Fatal => return 5;
      end case;
   end Level_Rank;

   function Trimmed (Value : String) return String is
      package F renames Ada.Strings.Fixed;
   begin
      return F.Trim (Value, Ada.Strings.Both);
   end Trimmed;

   function Load_File (Path : String) return String is
      File   : File_Type;
      Line   : String (1 .. 1024);
      Last   : Natural;
      Buffer : Unbounded_String := To_Unbounded_String ("");
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         Buffer := Buffer & Line (1 .. Last) & Character'Val (10);
      end loop;
      Close (File);
      return To_String (Buffer);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Load_File;

   procedure Append_To_File (Path : String; Line : String) is
      File : File_Type;
   begin
      if Path'Length = 0 then
         return;
      end if;
      declare
         Dir : constant String := Ada.Directories.Containing_Directory (Path);
      begin
         if Dir'Length > 0 and then not Ada.Directories.Exists (Dir) then
            Ada.Directories.Create_Path (Dir);
         end if;
      end;

      if Ada.Directories.Exists (Path) then
         Open (File, Append_File, Path);
      else
         Create (File, Out_File, Path);
      end if;
      Put_Line (File, Line);
      Close (File);
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Append_To_File;

   function Extract_Field_Start (Key : String; Source : String) return Integer is
      package F renames Ada.Strings.Fixed;
      Pattern : constant String := '"' & Key & '"';
      Pos     : Integer := F.Index (Source, Pattern);
   begin
      if Pos = 0 then
         return 0;
      end if;
      Pos := Pos + Pattern'Length;
      while Pos <= Source'Length and then Source (Pos) /= ':' loop
         Pos := Pos + 1;
      end loop;
      if Pos > Source'Length then
         return 0;
      end if;
      Pos := Pos + 1;
      while Pos <= Source'Length and then Ada.Characters.Handling.Is_Space (Source (Pos)) loop
         Pos := Pos + 1;
      end loop;
      return Pos;
   end Extract_Field_Start;

   function Parse_String_Value (Start_Pos : Integer; Source : String; Is_Null : out Boolean) return String is
      Pos : Integer := Start_Pos;
      package F renames Ada.Strings.Fixed;
   begin
      Is_Null := False;
      if Pos = 0 then
         return "";
      end if;
      if Pos <= Source'Length and then Source (Pos) = 'n' then
         if Source'Length - Pos + 1 >= 4 and then Source (Pos .. Pos + 3) = "null" then
            Is_Null := True;
            return "";
         end if;
      end if;
      if Pos > Source'Length or else Source (Pos) /= '"' then
         raise Constraint_Error with "Expected string literal";
      end if;
      Pos := Pos + 1;
      declare
         Rest    : constant String := Source (Pos .. Source'Length);
         Closing : constant Integer := F.Index (Rest, '"');
      begin
         if Closing = 0 then
            raise Constraint_Error with "Unterminated string";
         end if;
         return Rest (Rest'First .. Rest'First + Closing - 2);
      end;
   end Parse_String_Value;

   function Parse_Boolean_Value (Start_Pos : Integer; Source : String; Default : Boolean) return Boolean is
      Pos : Integer := Start_Pos;
   begin
      if Pos = 0 then
         return Default;
      end if;
      if Source'Length - Pos + 1 >= 4 and then Source (Pos .. Pos + 3) = "true" then
         return True;
      elsif Source'Length - Pos + 1 >= 5 and then Source (Pos .. Pos + 4) = "false" then
         return False;
      else
         raise Constraint_Error with "Invalid boolean";
      end if;
   end Parse_Boolean_Value;

   function Parse_Level_Name (Name : String) return Log_Level is
      Lower : constant String := Ada.Characters.Handling.To_Lower (Trimmed (Name));
   begin
      if Lower = "trace" then
         return Trace;
      elsif Lower = "debug" then
         return Debug;
      elsif Lower = "info" then
         return Info;
      elsif Lower = "warn" or else Lower = "warning" then
         return Warn;
      elsif Lower = "error" then
         return Error;
      elsif Lower = "fatal" then
         return Fatal;
      else
         raise Constraint_Error with "Unknown level: " & Name;
      end if;
   end Parse_Level_Name;

   procedure Parse_Level (Source : String; Threshold : out Log_Level; Tag : out Boolean) is
      Pos : constant Integer := Extract_Field_Start ("level", Source);
      package F renames Ada.Strings.Fixed;
   begin
      if Pos = 0 then
         raise Constraint_Error with "Missing level";
      end if;
      if Source (Pos) = '"' then
         declare
            Is_Null : Boolean := False;
            Value   : constant String := Parse_String_Value (Pos, Source, Is_Null);
         begin
            Threshold := Parse_Level_Name (Value);
            Tag := True;
         end;
      elsif Source (Pos) = '{' then
         declare
            Closing : Integer := Pos;
            Depth   : Integer := 0;
         begin
            while Closing <= Source'Length loop
               if Source (Closing) = '{' then
                  Depth := Depth + 1;
               elsif Source (Closing) = '}' then
                  Depth := Depth - 1;
                  exit when Depth = 0;
               end if;
               Closing := Closing + 1;
            end loop;
            if Depth /= 0 then
               raise Constraint_Error with "Invalid level object";
            end if;
            declare
               Obj : constant String := Source (Pos .. Closing);
               Threshold_Pos : constant Integer := Extract_Field_Start ("threshold", Obj);
               Tag_Pos       : constant Integer := Extract_Field_Start ("tag", Obj);
               Is_Null       : Boolean := False;
               Value         : constant String := Parse_String_Value (Threshold_Pos, Obj, Is_Null);
            begin
               Threshold := Parse_Level_Name (Value);
               if Tag_Pos = 0 then
                  Tag := True;
               else
                  Tag := Parse_Boolean_Value (Tag_Pos, Obj, True);
               end if;
            end;
         end;
      else
         raise Constraint_Error with "Unsupported level value";
      end if;
   end Parse_Level;

   procedure Initialize (Path : String) is
      Content : constant String := Load_File (Path);
      File_Pos : Integer;
      Net_Pos  : Integer;
      Is_Null  : Boolean := False;
   begin
      Current_Config := (Threshold => Info,
                         Tag_Level => True,
                         Timestamp => True,
                         File_Location => To_Unbounded_String (""),
                         Network_File_Location => To_Unbounded_String (""),
                         Has_File => False,
                         Has_Network => False);

      Parse_Level (Content, Current_Config.Threshold, Current_Config.Tag_Level);
      Current_Config.Timestamp := Parse_Boolean_Value (Extract_Field_Start ("timestamp", Content), Content, True);

      File_Pos := Extract_Field_Start ("file_location", Content);
      declare
         Value : constant String := Parse_String_Value (File_Pos, Content, Is_Null);
      begin
         if not Is_Null and then Value'Length > 0 then
            Current_Config.File_Location := To_Unbounded_String (Value);
            Current_Config.Has_File := True;
         end if;
      end;

      Net_Pos := Extract_Field_Start ("network_file_location", Content);
      declare
         Value : constant String := Parse_String_Value (Net_Pos, Content, Is_Null);
      begin
         if not Is_Null and then Value'Length > 0 then
            Current_Config.Network_File_Location := To_Unbounded_String (Value);
            Current_Config.Has_Network := True;
         end if;
      end;

      Configured := True;
   end Initialize;

   procedure Log_Message (Level : Log_Level; Message : String) is
      Line : String := "";
   begin
      if not Configured then
         raise Program_Error with "LibLogit not configured";
      end if;
      if Level_Rank (Level) < Level_Rank (Current_Config.Threshold) then
         return;
      end if;

      if Current_Config.Timestamp then
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            Line := Ada.Calendar.Formatting.Image (Now, Include_Time_Fraction => False) & " ";
         end;
      end if;
      if Current_Config.Tag_Level then
         Line := Line & Level_To_String (Level) & " ";
      end if;
      Line := Line & Message;

      Put_Line (Line);
      if Current_Config.Has_File then
         Append_To_File (To_String (Current_Config.File_Location), Line);
      end if;
      if Current_Config.Has_Network then
         Append_To_File (To_String (Current_Config.Network_File_Location), Line);
      end if;
   end Log_Message;

end LibLogit;
