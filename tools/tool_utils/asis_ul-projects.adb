------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                     A S I S _ U L . P R O J E C T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------
pragma Ada_2012;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Text_IO;              use Ada.Text_IO;

with GNAT.Directory_Operations;

with GNATCOLL.Projects.Aux;
with GNATCOLL.Traces;

with Table;

with ASIS_UL.Common;           use ASIS_UL.Common;
with ASIS_UL.Compiler_Options; use ASIS_UL.Compiler_Options;
with ASIS_UL.Environment;      use ASIS_UL.Environment;
with ASIS_UL.Options;          use ASIS_UL.Options;
with ASIS_UL.Output;           use ASIS_UL.Output;
with ASIS_UL.Source_Table;     use ASIS_UL.Source_Table;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;

package body ASIS_UL.Projects is

   Project_Env      : Project_Environment_Access;
   Project_File_Set : Boolean := False;

   Config_File_Name  : String_Access;
   Mapping_File_Name : String_Access;

   RTS_Path          : String_Access;
   --  Used to store path to RTS specified as a parameter of '--RTS=...' tool
   --  option. This is needed before the start of argument project file
   --  analysis

   -------------------------------
   --  External variables table --
   -------------------------------

   type X_Var_Record is record
      Var_Name          : String_Access;
      Var_Value         : String_Access;
      From_Command_Line : Boolean;
   end record;

   function "<" (Left, Right : X_Var_Record) return Boolean is
     (To_Lower (Left.Var_Name.all) < To_Lower (Right.Var_Name.all));

   function "=" (Left, Right : X_Var_Record)  return Boolean is
     (To_Lower (Left.Var_Name.all) = To_Lower (Right.Var_Name.all));

   package X_Vars_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => X_Var_Record);

   X_Vars : X_Vars_Sets.Set;

   ---------------------------------------------------------------
   -- Table needed to compose a list of switches to call a tool --
   ---------------------------------------------------------------

   package Tool_Switches is new Table.Table (
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Tool options");

   Program_Output_File : File_Type;
   --  File used to redirect the program output into.

   ------------------------
   --  Local subprograms --
   ------------------------

   function Needed_For_Tree_Creation (Option : String) return Boolean;
   --  Checks if the argument is the compilation option that is needed for
   --  tree creation

   function Is_Ada_File
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean;
   --  Checks if the given source file is an Ada file.

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean;
   --  Checks if the given source file belongs to an externally build library.

   procedure Recompute_View_Errors (S : String);
   --  Print out all errors but the warnings about missing directories.

   ------------
   --  Debug --
   ------------

   procedure Print_Debug_Info (I : File_Info);
   --  Prints out the debug info from the argument

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up (My_Project : Arg_Project_Type) is
      Root_Prj : Project_Type;
   begin
      if not Debug_Flag_N
        and then
         Is_Specified (My_Project)
      then
         Root_Prj := Root_Project (My_Project);
         GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Root_Prj);
      end if;
   end Clean_Up;

   -------------------------------
   -- Create_Configuration_File --
   -------------------------------

   procedure Create_Configuration_File (My_Project : Arg_Project_Type) is
      Config_Name : constant String :=
        GNATCOLL.Projects.Aux.Create_Config_Pragmas_File
          (My_Project.Root_Project);
   begin
      Store_Config_File_Name (Config_Name);
      Store_Option ("-gnatec=" & Config_Name);
   end Create_Configuration_File;

   -------------------------
   -- Create_Mapping_File --
   -------------------------

   procedure Create_Mapping_File (My_Project : Arg_Project_Type) is
      Mapping_Name : constant String :=
        GNATCOLL.Projects.Aux.Create_Ada_Mapping_File
          (My_Project.Root_Project);
   begin
      Store_Mapping_File_Name (Mapping_Name);
      Store_Option ("-gnatem=" & Mapping_Name);
   end Create_Mapping_File;

   ------------------------------------
   -- Extract_Compilation_Attributes --
   ------------------------------------

   procedure Extract_Compilation_Attributes
     (My_Project : in out Arg_Project_Type)
   is
      Proj      : Project_Type := My_Project.Root_Project;
      Attr_Proj : Project_Type;

      --  Attributes to check:
      Builder_Global_Configuration_Pragmas : constant Attribute_Pkg_String :=
        Build (Builder_Package, "Global_Configuration_Pragmas");
      Builder_Global_Config_File : constant Attribute_Pkg_String :=
        Build (Builder_Package, "Global_Config_File");

      Needs_RTS : Boolean := False;
   begin

      if Has_Attribute (Proj, Builder_Global_Configuration_Pragmas) then
         Attr_Proj := Attribute_Project
                        (Project   => Proj,
                         Attribute => Builder_Global_Configuration_Pragmas);

         declare
            Attr_Val : constant String :=
              Attribute_Value (Proj, Builder_Global_Configuration_Pragmas);
         begin
            Store_Option
              ("-gnatec=" &
               Normalize_Pathname
                 (Name      => Attr_Val,
                  Directory =>
                    GNAT.Directory_Operations.Dir_Name
                      (Display_Full_Name (Project_Path (Attr_Proj)))));
         end;
      end if;

      if Has_Attribute (Proj, Builder_Global_Config_File, "ada") then
         Attr_Proj := Attribute_Project
                        (Project   => Proj,
                         Index     => "ada",
                         Attribute => Builder_Global_Config_File);

         declare
            Attr_Val : constant String :=
              Attribute_Value (Proj, Builder_Global_Config_File, "ada");
         begin

            Store_Option
              ("-gnatec=" &
               Normalize_Pathname
                 (Name      => Attr_Val,
                  Directory =>
                    GNAT.Directory_Operations.Dir_Name
                      (Display_Full_Name (Project_Path (Attr_Proj)))));
         end;
      end if;

      Needs_RTS := Has_Attribute (Proj, Runtime_Attribute, Index => "Ada");

      while not Needs_RTS
          and then
            Proj /= No_Project
      loop
         Proj := Extended_Project (Proj);
         Needs_RTS := Has_Attribute (Proj, Runtime_Attribute, Index => "Ada");
      end loop;

      if Needs_RTS then
         --???
         --  There is some code duplication with
         --  ASIS_UL.Compiler_Options.Get_Full_Path_To_RTS, needs refactoring
         declare
            Dirs : constant File_Array := Project_Env.Predefined_Object_Path;
            Idx  : Natural;
         begin
            for J in Dirs'Range loop
               Idx := Index (Dirs (J).Display_Full_Name, "adalib");

               if Idx /= 0 then
                  declare
                     Result : constant String   := Dirs (J).Display_Full_Name;
                     F_Idx  : constant Positive := Result'First;
                  begin
                     Store_Option
                       ("--RTS=" & Trim (Result (F_Idx .. Idx - 2), Both));

                     Custom_RTS := new String'(Get_Runtime (Proj));

                     goto Done;
                  end;
               end if;
            end loop;

            Error ("cannot detect the full path to runtime " &
                   "from Runtime attribute");
            raise Fatal_Error;
            <<Done>> null;
         end;
      end if;

   end Extract_Compilation_Attributes;

   --------------------------
   -- Extract_Tool_Options --
   --------------------------

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type) is
      Arg_File_Name         : String_Access;

      Proj          : constant Project_Type := Root_Project (My_Project);

      Attr_Switches : constant Attribute_Pkg_List :=
        Build (Tool_Package_Name (Arg_Project_Type'Class (My_Project)),
              "Switches");
      Attr_Def_Switches : constant Attribute_Pkg_List
        := Build (Tool_Package_Name (Arg_Project_Type'Class (My_Project)),
                  "Default_Switches");

      Attr_Indexes  : String_List_Access;
      Tool_Switches : String_List_Access;
      Index_Found   : Boolean := False;

      Options_Defined : Boolean := False;

      Proj_Args_Parser : Opt_Parser;

   begin
      if Files_In_Temp_Storage = 1 then
         Arg_File_Name := new String'(First_File_In_Temp_Storage);

         Attr_Indexes :=
           new String_List'(Attribute_Indexes (Proj, Attr_Switches));

         for J in  Attr_Indexes'Range loop
            if Arg_File_Name.all = Attr_Indexes (J).all then
               --  What about non-case-sensitive system?
               Index_Found := True;
               exit;
            end if;
         end loop;
      end if;

      if not Index_Found then
         --  We have to get tool options from Default_Sources

         if Has_Attribute (Proj, Attr_Def_Switches, "ada") then
            Tool_Switches := Attribute_Value (Proj, Attr_Def_Switches, "ada");
            Options_Defined := True;
         end if;
      else
         if Has_Attribute (Proj, Attr_Switches) then
            Tool_Switches :=
              Attribute_Value (Proj, Attr_Switches, Arg_File_Name.all);
            Options_Defined := True;
         end if;
      end if;

      if Options_Defined then
         Initialize_Option_Scan
           (Parser                   => Proj_Args_Parser,
            Command_Line             => Tool_Switches,
            Switch_Char              => '-',
            Stop_At_First_Non_Switch => False,
            Section_Delimiters       => My_Project.Get_Section_Delimiters);

         Scan_Arguments
           (My_Project  => Arg_Project_Type'Class (My_Project),
            Parser      => Proj_Args_Parser,
            In_Switches => Index_Found);

      end if;

   end Extract_Tool_Options;

   --------------------------
   -- Get_Config_File_Name --
   --------------------------

   function Get_Config_File_Name return String is
   begin
      if Config_File_Name = null then
         return "";
      else
         return Config_File_Name.all;
      end if;
   end Get_Config_File_Name;

   ---------------------------
   -- Get_Mapping_File_Name --
   ---------------------------

   function Get_Mapping_File_Name return String is
   begin
      if Mapping_File_Name = null then
         return "";
      else
         return Mapping_File_Name.all;
      end if;
   end Get_Mapping_File_Name;

   ------------------
   -- Get_RTS_Path --
   ------------------

   function Get_RTS_Path return String is
   begin
      if RTS_Path = null then
         return "";
      else
         return RTS_Path.all;
      end if;
   end Get_RTS_Path;

   ------------------------------
   -- Get_Sources_From_Project --
   ------------------------------

   procedure Get_Sources_From_Project
     (My_Project : Arg_Project_Type)
   is
      Prj      : Project_Type;
      Files    : File_Array_Access;
      Success  : Boolean := False;

   begin
      if Compute_Project_Closure (Arg_Project_Type'Class (My_Project))
        and then
           (ASIS_UL.Options.No_Argument_File_Specified
          or else
            (U_Option_Set and then not File_List_Specified))
      then
         if Main_Unit = null then
            Prj := My_Project.Root_Project;

            Files := Prj.Source_Files (Recursive => U_Option_Set);

            for F in Files'Range loop
               if not Is_Externally_Built (Files (F), My_Project)
                 and then
                  Is_Ada_File (Files (F), My_Project)
               then
                  ASIS_UL.Source_Table.Store_Sources_To_Process
                    (Files (F).Display_Base_Name);
               end if;
            end loop;

            if U_Option_Set then
               if Files'Length = 0 then
                  Error (My_Project.Source_Prj.all &
                         "does not contain source files");
                  return;
               end if;
            else
               Prj := Extended_Project (Prj);

               while Prj /= No_Project loop
                  Unchecked_Free (Files);
                  Files := Prj.Source_Files (Recursive => False);

                  for F in Files'Range loop
                     if not Is_Externally_Built (Files (F), My_Project)
                       and then
                        Is_Ada_File (Files (F), My_Project)
                     then
                        ASIS_UL.Source_Table.Store_Sources_To_Process
                          (Files (F).Display_Base_Name);
                     end if;
                  end loop;

                  Prj := Extended_Project (Prj);
               end loop;

            end if;

         else
            Store_Files_From_Binder_Output (My_Project, Success);
         end if;
      end if;
   end Get_Sources_From_Project;

   ----------------------------
   -- Initialize_Environment --
   ----------------------------

   procedure Initialize_Environment is
      Firts_Idx : constant Natural := Tool_Name'First;
      Last_Idx  : constant Natural :=
        Index (Tool_Name.all, "-", Ada.Strings.Backward);

   begin
      GNATCOLL.Traces.Parse_Config_File;
      Initialize (Project_Env);

      Project_Env.Set_Target_And_Runtime
        (Tool_Name (Firts_Idx .. Last_Idx - 1),
         Get_RTS_Path);

      if Follow_Symbolic_Links then
         Project_Env.Set_Trusted_Mode (True);
      end if;

      Set_Automatic_Config_File (Project_Env.all);

   end Initialize_Environment;

   -----------------
   -- Is_Ada_File --
   -----------------

   function Is_Ada_File
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean
   is
   begin
      return To_Lower (Language (Info (My_Project, File))) = "ada";
   end Is_Ada_File;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean
   is
      F_Info : constant File_Info    := Info (My_Project, File);
      Proj   : constant Project_Type := Project (F_Info);
      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      if Has_Attribute (Proj, Attr) then
         if Attribute_Value (Proj, Attr) = "true" then
            return True;
         end if;
      end if;
      return False;
   end Is_Externally_Built;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean is
   begin
      return My_Project.Source_Prj /= null;
   end Is_Specified;

   -----------------------
   -- Load_Tool_Project --
   -----------------------

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type) is
      procedure Errors (S : String);
      procedure Errors (S : String) is
      begin
         if Index (S, " not a regular file") /= 0 then
            Error ("project file " & My_Project.Source_Prj.all & " not found");

         else
            Error (S);
         end if;
      end Errors;
   begin
      My_Project.Load
        (GNATCOLL.VFS.Create (+My_Project.Source_Prj.all),
         Project_Env,
         Errors         => Errors'Unrestricted_Access,
         Recompute_View => False);

      if Is_Aggregate_Project (My_Project.Root_Project) then
         Error ("aggregate projects are not supported");
         raise Parameter_Error;
      end if;
   exception
      when Invalid_Project =>
         raise Parameter_Error;
   end Load_Tool_Project;

   ------------------------------
   -- Needed_For_Tree_Creation --
   ------------------------------

   function Needed_For_Tree_Creation (Option : String) return Boolean is
      Result    : Boolean          := False;
      First_Idx : constant Natural := Option'First;
   begin
      --  a simple prototype for the moment, to be completed...
      if Option = "-gnat83"
        or else
         Option = "-gnat95"
        or else
         Option = "-gnat05"
        or else
         Option = "-gnat12"
        or else
         Option = "-gnatdm"
        or else
         Option = "-gnatd.V"
        or else
         (Option'Length >= 10
         and then
          Option (First_Idx .. First_Idx + 6) = "-gnateD"
         and then
          Preprocessing_Allowed)
        or else
         (Option'Length >= 10
         and then
          Option (First_Idx .. First_Idx + 6) = "-gnatep"
         and then
          Preprocessing_Allowed)
        or else
         Option = "-gnatI"
        or else
         (Option'Length >= 7
         and then
          Option (First_Idx .. First_Idx + 5) = "--RTS=")

--        or else
--         Option = ""
      then
         Result := True;
      end if;

      return Result;
   end Needed_For_Tree_Creation;

   ----------------------
   -- Print_Debug_Info --
   ----------------------

   procedure Print_Debug_Info (I : File_Info) is
   begin
      Info ("  Unit_Part " & Unit_Part (I)'Img);
      Info ("  Unit_Name " & Unit_Name (I));
      Info ("  File      " & Display_Base_Name (File (I)));
   end Print_Debug_Info;

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File
     (My_Project : in out Arg_Project_Type'Class)
   is
   begin

      if not My_Project.Is_Specified then
         return;
      end if;

      Register_Tool_Attributes       (My_Project);
      Initialize_Environment;
      Load_Tool_Project              (My_Project);
      Set_External_Values            (My_Project);
      Extract_Compilation_Attributes (My_Project);
      Extract_Tool_Options           (My_Project);
      Get_Sources_From_Project       (My_Project);
      Create_Mapping_File            (My_Project);
      Create_Configuration_File      (My_Project);
   end Process_Project_File;

   ---------------------------
   -- Recompute_View_Errors --
   ---------------------------

   procedure Recompute_View_Errors (S : String) is
   begin
      if Index (S, "warning") /= 0
        and then Index (S, "directory") /= 0
        and then Index (S, "not found") /= 0
      then
         return;
      else
         Put_Line (S);
      end if;
   end Recompute_View_Errors;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      pragma Unreferenced (In_Switches, Parser, First_Pass, My_Project);
   begin
      Error ("Scan_Arguments procedure should be defined for the tool!");
      raise Fatal_Error;
   end Scan_Arguments;

   ------------------------
   -- Section_Delimiters --
   ------------------------

   function Section_Delimiters (My_Project : Arg_Project_Type) return String is
      pragma Unreferenced (My_Project);
   begin
      return "cargs asis-tool-args";
      --  The undocumented -asis-tool-args section is used in incremental mode
      --  to pass extra args *after* the other section(s), such as -cargs down
      --  to the inner invocations of the tool.
   end Section_Delimiters;

   function Get_Section_Delimiters
     (My_Project : Arg_Project_Type'Class) return String is
      Delim : constant String := My_Project.Section_Delimiters;
   begin
      return
         (if Mimic_gcc
            then Replace_String
              (Delim, From => "cargs",
               To => "inner-cargs") -- See doc in asis_ul-environment.adb
            else Delim);
   end Get_Section_Delimiters;

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : in out Arg_Project_Type) is
      Vars : Scenario_Variable_Array := My_Project.Scenario_Variables;
      use X_Vars_Sets;
      C        : Cursor;
      Next_Var : X_Var_Record;
   begin
      C := First (X_Vars);

      while Has_Element (C) loop

         Next_Var := Element (C);

         for I in Vars'Range loop

            if External_Name (Vars (I)) = Next_Var.Var_Name.all then

               declare
                  Pos_Vals : constant String_List :=
                    My_Project.Possible_Values_Of (Vars (I));
                  Present : Boolean := False;
               begin

                  for J in Pos_Vals'Range loop
                     if Pos_Vals (J).all = Next_Var.Var_Value.all then
                        Present  := True;
                        exit;
                     end if;
                  end loop;

                  if not Present then
                     Error
                       ("value " & Next_Var.Var_Value.all &
                        " is illegal for " & Next_Var.Var_Name.all);
                     raise Parameter_Error;
                  end if;

               end;

               Set_Value (Vars (I), Next_Var.Var_Value.all);
               exit;
            end if;

         end loop;

         C := Next (C);

      end loop;

      My_Project.Change_Environment (Vars);
      My_Project.Recompute_View
        (Errors => Recompute_View_Errors'Unrestricted_Access);

   end Set_External_Values;

   ----------------------------
   -- Set_Global_Result_Dirs --
   ----------------------------

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type) is
      Global_Report_Dir : Virtual_File;
   begin

      if not No_Object_Dir then
         if Subdir_Name /= null then
            Set_Object_Subdir (Project_Env.all, +Subdir_Name.all);
            Recompute_View
              (My_Project,
               Errors => Recompute_View_Errors'Unrestricted_Access);
         end if;

         Global_Report_Dir := My_Project.Root_Project.Object_Dir;

         if Global_Report_Dir = No_File then
            Global_Report_Dir := My_Project.Root_Project.Project_Path;
         end if;

         Set_Global_Report_Dir (Display_Dir_Name (Global_Report_Dir));
      end if;

   end Set_Global_Result_Dirs;

   -----------------------------------
   -- Set_Individual_Source_Options --
   -----------------------------------

   procedure Set_Individual_Source_Options (My_Project : Arg_Project_Type) is
      Sources : constant File_Array_Access :=
        My_Project.Root_Project.Source_Files (Recursive => True);

      Per_File_Output_Needed : constant Boolean :=
        Needs_Per_File_Output (Arg_Project_Type'Class (My_Project));

      Project_U   : Project_Type;
      Attr_Proj   : Project_Type;
      Source_Info : File_Info;

      Sws        : String_List_Access;
      Is_Default : Boolean := False;
      SF         : SF_Id;

      File_Switches : String_Access;
      Tmp           : String_Access;

      procedure Scan_Switches;
      --  Works on Sws as on a global object. Scans the argument, checks if
      --  the element being visited is needed for tree creation, and if it is,
      --  stores it in File_Switches

      procedure Add_Switch (S : String);
      --  Adds S to File_Switches;

      Compiler_Local_Configuration_Pragmas : constant Attribute_Pkg_String :=
        Build (Compiler_Package, "Local_Configuration_Pragmas");
      Compiler_Local_Config_File : constant Attribute_Pkg_String :=
        Build (Compiler_Package, "Local_Config_File");

      function Normalize_Switch (S : String) return String;
      --  If the switch contains a path, normalizes this path. This is needed
      --  because the switch will be used from the temporary directory created
      --  by a tool

      procedure Add_Switch (S : String) is
      begin
         if File_Switches = null then
            File_Switches := new String'(S);
         else
            Tmp := new String'(File_Switches.all & ' ' & S);
            Free (File_Switches);
            File_Switches := new String'(Tmp.all);
            Free (Tmp);
         end if;
      end Add_Switch;

      procedure Scan_Switches is
      begin
         for J in Sws'Range loop
            if ASIS_UL.Debug.Debug_Flag_C then
               Info_No_EOL (Sws (J).all & ' ');
            end if;

            if Needed_For_Tree_Creation (Sws (J).all) then
               Add_Switch (Normalize_Switch (Sws (J).all));
            end if;
         end loop;

         if ASIS_UL.Debug.Debug_Flag_C then
            if Is_Default then
               Info_No_EOL ("(default)");
            end if;

            Info ("");
         end if;

         Free (Sws);
      end Scan_Switches;

      function Normalize_Switch (S : String) return String is
         Res : constant String := Trim (S, Both);
         Opt_Start  : constant Natural := S'First;
         Opt_End    :          Natural;
         Path_Start :          Natural;
         Path_End   : constant Natural := S'Last;
      begin
         if Res'Length >= 9
           and then
            Res (Opt_Start .. Opt_Start + 5) = "-gnate"
           and then
            Res (Opt_Start + 6) in 'e' | 'p'
         then
            Opt_End    := Opt_Start + 6;
            Path_Start := Opt_End + 1;

            while Path_Start < Path_End and then
                  Res (Path_Start) in ' ' | '='
            loop
               Path_Start := Path_Start + 1;
            end loop;

            return Res (Opt_Start .. Opt_End) &
                        Normalize_Pathname (Res (Path_Start .. Path_End));
         else
            return Res;
         end if;
      end Normalize_Switch;

   begin
      for S in Sources'Range loop
         Source_Info  := My_Project.Info (Sources (S));
         Project_U    := Project (Source_Info);

         SF :=
           File_Find (Display_Base_Name (Sources (S)), Use_Short_Name => True);

         if Present (SF) then

            if ASIS_UL.Debug.Debug_Flag_C then
               Info ("Switches defined for " & Short_Source_Name (SF));

               if ASIS_UL.Debug.Debug_Flag_P then
                  Print_Debug_Info (Source_Info);
               end if;
            end if;

            Switches
              (Project          => Project_U,
               In_Pkg           => Compiler_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            Switches
              (Project          => Project_U,
               In_Pkg           => Builder_Package,
               File             => Sources (S),
               Language         => "ada",
               Value            => Sws,
               Is_Default_Value => Is_Default);

            Scan_Switches;

            if not U_Option_Set
              and then
               Has_Attribute
                 (Project_U, Compiler_Local_Configuration_Pragmas)
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Configuration_Pragmas);
               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U, Compiler_Local_Configuration_Pragmas);
               begin
                  Add_Switch
                    ("-gnatec=" &
                     Normalize_Pathname
                       (Name      => Attr_Val,
                        Directory =>
                          GNAT.Directory_Operations.Dir_Name
                           (Display_Full_Name (Project_Path (Attr_Proj)))));
               end;
            end if;

            if not U_Option_Set
              and then
               Has_Attribute
                 (Project_U, Compiler_Local_Config_File, "ada")
            then
               Attr_Proj :=
                 Attribute_Project
                   (Project   => Project_U,
                    Attribute => Compiler_Local_Config_File,
                    Index     => "ada");

               declare
                  Attr_Val : constant String :=
                    Attribute_Value
                      (Project_U, Compiler_Local_Config_File, "ada");
               begin
                  Add_Switch
                    ("-gnatec=" &
                     Normalize_Pathname
                       (Name      => Attr_Val,
                        Directory =>
                          GNAT.Directory_Operations.Dir_Name
                            (Display_Full_Name (Project_Path (Attr_Proj)))));
               end;
            end if;

            if File_Switches /= null then
               Add_Compilation_Switches
                 (SF, Argument_String_To_List (File_Switches.all));

               if ASIS_UL.Debug.Debug_Flag_C then
                  Info ("Stored switches " & File_Switches.all);
               end if;

               Free (File_Switches);
            elsif ASIS_UL.Debug.Debug_Flag_C then
               Info ("No stored switches");
            end if;

            --  Defining the directory to place the file-specific results into:

            if not No_Object_Dir
              and then
               Per_File_Output_Needed
            then
               Set_Result_Dir
                 (SF,
                  Source_Result_Dir
                    (Arg_Project_Type'Class (My_Project),
                     Project_U,
                     Sources (S)));
            end if;
         end if;
      end loop;
   end Set_Individual_Source_Options;

   ---------------------
   -- Set_Subdir_Name --
   ---------------------

   procedure Set_Subdir_Name (S : String) is
   begin
      Free (Subdir_Name);
      Subdir_Name := new String'(S);

   end Set_Subdir_Name;

   ----------------
   -- Source_Prj --
   ----------------

   function Source_Prj (My_Project : Arg_Project_Type) return String is
   begin
      if Is_Specified (My_Project) then
         return My_Project.Source_Prj.all;
      else
         return "";
      end if;
   end Source_Prj;

   -----------------------
   -- Source_Result_Dir --
   -----------------------

   function Source_Result_Dir
     (My_Project  : Arg_Project_Type;
      Source_Prj  : Project_Type;
      Source_File : Virtual_File)
   return           String
   is
      pragma Unreferenced (Source_File);
      Report_Dir : Virtual_File;
   begin
      Report_Dir := Source_Prj.Object_Dir;

      if Report_Dir = No_File then
         Report_Dir := My_Project.Root_Project.Project_Path;
      end if;

      return Display_Dir_Name (Report_Dir);
   end Source_Result_Dir;

   ------------------------------------
   -- Store_Files_From_Binder_Output --
   ------------------------------------

   procedure Store_Files_From_Binder_Output
     (My_Project :        Arg_Project_Type;
      Success    : in out Boolean)
   is
      Command     :           String_Access;
      Target      : constant String := Detect_Target;
      Return_Code :           Integer;

      Bind_Out_File_Name : constant String := Tool_Temp_Dir.all & ".bind_out";
      Tmp_Success        : Boolean;
      pragma Unreferenced (Tmp_Success);

      Max_Str_Len : constant Positive := 1024;
      Str_Buff    : String (1 .. Max_Str_Len);
      Str_Len     : Natural;

      use X_Vars_Sets;
      C        : Cursor;
      Next_Var : X_Var_Record;
   begin
      --  Define the name of the command to call:
      Command := Locate_Exec_On_Path ("gprbuild");

      Tool_Switches.Init;

      Tool_Switches.Append (new String'("-f"));
      Tool_Switches.Append (new String'("-q"));
      Tool_Switches.Append (new String'("-b"));

      if Target /= "" then
         Tool_Switches.Append
           (new String'("--target=" &
                        Target (Target'First .. Target'Last - 1)));
      end if;

      Tool_Switches.Append (new String'("-P"));
      Tool_Switches.Append (new String'(My_Project.Source_Prj.all));

      C := First (X_Vars);

      while Has_Element (C) loop

         Next_Var := Element (C);

         Tool_Switches.Append (new String'
           ("-X" & Next_Var.Var_Name.all & "=" & Next_Var.Var_Value.all));

         C := Next (C);
      end loop;

      if Get_RTS_Path /= "" then
         Tool_Switches.Append (new String'("--RTS=" & Get_RTS_Path));
      end if;

      Tool_Switches.Append (new String'("-gnatws"));

      Tool_Switches.Append (new String'(Main_Unit.all));

      Tool_Switches.Append (new String'("-bargs"));

      Tool_Switches.Append (new String'("-ws"));
      Tool_Switches.Append (new String'("-R"));
      Tool_Switches.Append (new String'("-Z"));

      if Debug_Flag_C then
         Info_No_EOL (Command.all);
         Info_No_EOL (" ");

         for J in 1 .. Tool_Switches.Last loop
            Info_No_EOL (Tool_Switches.Table (J).all);
            Info_No_EOL (" ");
         end loop;
      end if;

      Spawn
        (Program_Name => Command.all,
         Args         => String_List
           (Tool_Switches.Table (1 .. Tool_Switches.Last)),
         Output_File  => Bind_Out_File_Name,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => False);

      --  Read source files

      if Success and then Return_Code = 0 then

         Open (Program_Output_File, In_File, Bind_Out_File_Name);

         while not  End_Of_File (Program_Output_File) loop
            Get_Line (Program_Output_File, Str_Buff, Str_Len);

            ASIS_UL.Source_Table.Store_Sources_To_Process
              (Fname       => Trim (Str_Buff (1 .. Str_Len), Both));
         end loop;

         Close (Program_Output_File);
      else
         Error_No_Tool_Name ("could not get closure of " & Main_Unit.all);
         raise Fatal_Error;
      end if;

      --  Clean-up
      Free (Command);

      if not (Debug_Mode or else Debug_Flag_N) then
         Delete_File (Bind_Out_File_Name, Tmp_Success);
      end if;

   exception
      when others =>
         if Is_Open (Program_Output_File) then
            Close (Program_Output_File);
         end if;

         if not (Debug_Mode or else Debug_Flag_N) then
            Delete_File (Bind_Out_File_Name, Tmp_Success);
         end if;

         raise;
   end Store_Files_From_Binder_Output;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Store_External_Variable
     (Var                  : String;
      Is_From_Command_Line : Boolean := True)
   is
      Var_Name_Start  : constant Natural := Var'First;
      Var_Name_End    :          Natural := Index (Var, "=");
      Var_Value_Start :          Natural;
      Var_Value_End   : constant Natural := Var'Last;

      New_Var_Rec : X_Var_Record;

      use X_Vars_Sets;
      C : Cursor;
   begin
      if Var_Name_End <= Var_Name_Start then
         Error ("wrong parameter of -X option: " & Var);
         raise Parameter_Error;
      else
         Var_Name_End    := Var_Name_End - 1;
         Var_Value_Start := Var_Name_End + 2;
         New_Var_Rec    :=
           (Var_Name  => new String'(Var (Var_Name_Start .. Var_Name_End)),
            Var_Value => new String'(Var (Var_Value_Start .. Var_Value_End)),
            From_Command_Line => Is_From_Command_Line);
      end if;

      C := Find (X_Vars, New_Var_Rec);

      if Has_Element (C) then
         if not Element (C).From_Command_Line
           or else
            Element (C).From_Command_Line = Is_From_Command_Line
         then
            Replace_Element (Container => X_Vars,
                             Position  => C,
                             New_Item  => New_Var_Rec);
         else
            Free (New_Var_Rec.Var_Name);
            Free (New_Var_Rec.Var_Value);
         end if;
      else
         Insert (X_Vars, New_Var_Rec);
      end if;
   end Store_External_Variable;

   ---------------------
   -- Store_Main_Unit --
   ---------------------

   procedure Store_Main_Unit
     (Unit_Name   : String;
      Store       : Boolean := True)
   is
   begin
      if ASIS_UL.Projects.Main_Unit = null then

         if Store then
            ASIS_UL.Projects.Main_Unit := new String'(Unit_Name);
         end if;
      else
         if Store then
            Error ("cannot specify more than one main after -U");
            raise Parameter_Error;
         end if;
      end if;
   end Store_Main_Unit;

   -----------------------------
   -- Store_Mapping_File_Name --
   -----------------------------

   procedure Store_Mapping_File_Name (S : String) is
   begin
      Free (Mapping_File_Name);
      Mapping_File_Name := new String'(S);
   end Store_Mapping_File_Name;

   ----------------------------
   -- Store_Config_File_Name --
   ----------------------------

   procedure Store_Config_File_Name (S : String) is
   begin
      Free (Config_File_Name);
      Config_File_Name := new String'(S);
   end Store_Config_File_Name;

   --------------------------
   -- Store_Project_Source --
   --------------------------

   procedure Store_Project_Source
     (My_Project         : in out Arg_Project_Type;
      Project_File_Name  : String) is

      Ext : constant String :=
        (if Has_Suffix (Project_File_Name, Suffix => ".gpr")
           then ""
           else ".gpr");
   begin
      if Project_File_Set then
         Error ("cannot have several project files specified");
         raise Parameter_Error;
      else
         Project_File_Set := True;
      end if;

      My_Project.Source_Prj := new String'(Project_File_Name & Ext);

   end Store_Project_Source;

   --------------------
   -- Store_RTS_Path --
   --------------------

   procedure Store_RTS_Path (S : String) is
   begin
      Free (RTS_Path);
      RTS_Path := new String'(S);
   end Store_RTS_Path;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   function Tool_Package_Name (My_Project : Arg_Project_Type) return String is
      pragma Unreferenced (My_Project);

      Result    : constant String  := Tool_Name.all;
      First_Idx : Natural := Index (Result, "-", Ada.Strings.Backward);
      Last_Idx  : constant Natural := Result'Last;

   begin
      if First_Idx = 0 then
         First_Idx := Tool_Name'First;
      end if;

      return Result (First_Idx .. Last_Idx);
   end Tool_Package_Name;

end ASIS_UL.Projects;
