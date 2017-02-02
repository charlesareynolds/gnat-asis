------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                       A S I S _ U L . C O M M O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2016, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with GNAT.Directory_Operations;

with ASIS_UL.Output;             use ASIS_UL.Output;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;

package body ASIS_UL.Common is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Tool_Name_And_Path;
   --  Reads the tool name from the command line and sets Tool_Name. If the
   --  tool name contains directory information, adds the directory to the
   --  path.

   Global_Report_Dir : String_Access := new String'("." & Directory_Separator);
   --  The name of the directory to place the global results into

   -------------------
   -- Detect_Target --
   -------------------

   function Detect_Target return String is
      use GNAT.Directory_Operations;

      Tgt_Last : constant Natural :=
        Index (Tool_Name.all, "-", Ada.Strings.Backward);
      AAMP_Idx : constant Natural := Index (Tool_Name.all, "gnaamp");
   begin

      if AAMP_Idx = Tool_Name'First then
         return "AAMP";
      elsif Tgt_Last > 0 then
         return Tool_Name (Tool_Name'First .. Tgt_Last);
      else
         return "";
      end if;

   exception
      when others =>
         return "";

   end Detect_Target;

   ---------------------------
   -- Get_Global_Report_Dir --
   ---------------------------

   function Get_Global_Report_Dir return String is
   begin
      return Global_Report_Dir.all;
   end Get_Global_Report_Dir;

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File (Project_File_Name : String) is
   begin
      --  pragma Assert (False, "???Process_Project_File is not used");
      --  Actually, it is still used by Ada2java. This should be replaced with
      --  proper project support as has been done for other ASIS tools.

      if Is_Regular_File (Project_File_Name) then
         Project_File_Obsolete :=
           new String'(Normalize_Pathname (Project_File_Name));

         if Project_Support_Type = No_Tmp_Project_File then
            Store_Option ("-P" & Project_File_Obsolete.all);
         end if;

      else
         Error ("the project file " & Project_File_Name & " not found");
         raise Parameter_Error;
      end if;

      Gcc_To_Call := Gnatmake_To_Call;

      if Gcc_To_Call /= null then
         Use_Gnatmake_To_Compile := True;
      else
         Error ("can not locate gnatmake to compile with a project file");
         raise Parameter_Error;
      end if;

      Use_Project_File_Obsolete := True;

   end Process_Project_File;

   ---------------------------
   -- Set_Global_Report_Dir --
   ---------------------------

   procedure Set_Global_Report_Dir (Dir : String) is
   begin
      Free (Global_Report_Dir);
      pragma Assert (Dir /= "");
      Global_Report_Dir := new String'(Dir & Directory_Separator);
   end Set_Global_Report_Dir;

   ----------------------------
   -- Set_Tool_Name_And_Path --
   ----------------------------

   procedure Set_Tool_Name_And_Path is
      Full_Tool_Name : constant String := Ada.Command_Line.Command_Name;
      Exe_Suffix     : String_Access   := Get_Executable_Suffix;
   begin
      Tool_Name :=
        new String'(To_Lower
                      (GNAT.Directory_Operations.Base_Name
                         (Full_Tool_Name, Suffix => Exe_Suffix.all)));

      for Index in reverse Full_Tool_Name'Range loop
         if Full_Tool_Name (Index) = Directory_Separator then
            declare
               Absolute_Dir : constant String :=
                                 Normalize_Pathname
                                  (Full_Tool_Name
                                     (Full_Tool_Name'First .. Index));

               PATH : constant String :=
                 Absolute_Dir & Path_Separator & Getenv ("PATH").all;

            begin
               Setenv ("PATH", PATH);
            end;

            exit;
         end if;
      end loop;

      Free (Exe_Suffix);
   end Set_Tool_Name_And_Path;

begin
   Set_Tool_Name_And_Path;

   --  Detetermine the name of the executables for gcc, gnatmake, and gprbuild

   declare
      Target        : constant String := Detect_Target;
      Gcc_Name      : constant String :=
        (if Target = "AAMP" then "gnaamp" else Target & "gcc");
      Gnatmake_Name : constant String :=
        (if Target = "AAMP" then "gnaampmake" else Target & "gnatmake");
      pragma Unreferenced (Gnatmake_Name);
      Gprbuild_Name : constant String := "gprbuild";
   begin
      Gcc_To_Call      := Locate_Exec_On_Path (Gcc_Name);
      Gprbuild_To_Call := Locate_Exec_On_Path (Gprbuild_Name);
      Gnatmake_To_Call := Gprbuild_To_Call;
   end;
end ASIS_UL.Common;
