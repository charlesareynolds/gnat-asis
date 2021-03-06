------------------------------------------------------------------------------
--                                                                          --
--                           GNATTEST COMPONENTS                            --
--                                                                          --
--                 G N A T T E S T . E N V I R O N M E N T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
--                                                                          --
-- GNATTEST  is  free  software;  you  can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software  Foundation;  either  version  2, or (at your option) any later --
-- version.  GNATTEST  is  distributed  in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public License distributed with GNAT; see file COPYING. If --
-- not, write to the  Free  Software  Foundation, 51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.,                                      --
--                                                                          --
-- GNATTEST is maintained by AdaCore (http://www.adacore.com).              --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Sets;

with Asis.Ada_Environments;      use Asis.Ada_Environments;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with GNAT.Command_Line;          use GNAT.Command_Line;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with System.Multiprocessors;

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Output;             use ASIS_UL.Output;

with GNATtest.Options;              use GNATtest.Options;
with GNATtest.Common;               use GNATtest.Common;
with GNATtest.Skeleton.Source_Table;    use GNATtest.Skeleton.Source_Table;
with GNATtest.Harness.Source_Table; use GNATtest.Harness.Source_Table;

with GNAT.Strings;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Projects.Aux;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

package body GNATtest.Environment is

   Me : constant Trace_Handle := Create ("Environment", Default => Off);

   Parameter_Error : exception;
   --  Is raised if the initialization is impossible or fails down because of
   --  any reason

   Output_M : Output_Mode := Default_Output_Mode;
   Output_M_Set : Boolean := False;

   Default_Skeletons_Set : Boolean := False;
   Show_Passed_Tests_Set : Boolean := False;
   Add_Exit_Status_Set   : Boolean := False;

   Run_Dir : String_Access;
   --  Directory from which the tool was called.

   Env   : Project_Environment_Access;

   Recursive_Sources : Boolean := False;

   Object_Dir : String_Access;

   Config_Name : constant String := "gnat.adc";

   --  Temporary source file name storage. After determining in which mode
   --  the tool works sources are to be transfered to corresponding source
   --  table.

   Main_Unit : String_Access := null;

   use List_Of_Strings;

   Source_Buffer  : List_Of_Strings.List;
   Ext_Var_Buffer : List_Of_Strings.List;
   SB_Cur         : List_Of_Strings.Cursor;

   --  Flags for default output dirs being set explicitly.
   Stub_Dir_Set   : Boolean := False;
   Tests_Dir_Set   : Boolean := False;
   Harness_Dir_Set : Boolean := False;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Scan_Parameters;
   --  Scans the command-line parameters and sets the metrics to compute and
   --  sources to process.

   procedure Check_Parameters;
   --  Checks that parameter settings are compatible fo generation mode.
   --  Raises Parameter_Error and generates the diagnostic message
   --  if the check fails.

   procedure Check_Aggregation_Parameters;
   --  Checks that parameter settings are compatible for aggregation mode.
   --  Raises Parameter_Error and generates the diagnostic message
   --  if the check fails.

   procedure Create_Temp_Dir (Obj_Dir : String := "");
   --  Creates the temporary directory and stores its name in Temp_Dir.

   procedure Brief_Help;
   --  Prints out the brief help.

   procedure Scan_Dir_Affix (Affix : String);
   --  Sets the values of Test_Dir_Prefix and Test_Dir_Suffix options.

   procedure Check_Subdir;
   --  Checks if there are no intersections between target and source dirs. If
   --  so, tries to create all target subdirs.

   procedure Check_Parallel;
   --  Checks if there are no intersections between target and source dirs. If
   --  so, tries to create all target parallel dirs.

   procedure Check_Separate_Root;
   --  Checks if there are no intersections between target and source dirs. If
   --  so, tries to create a directori hierarchy similar to one of the tested
   --  sources.

   procedure Check_Direct;
   --  Checks if there are no intersections between target and source dirs. If
   --  so, tries to create target dir.

   procedure Check_Stub;
   --  Checks if there are no intersections between stub and source dirs and
   --  between stub and test dirs.

   procedure Process_Exclusion_Lists;
   --  Process lists of units fo stub exclusion.

   function Is_Externally_Built (File : Virtual_File) return Boolean;
   --  Checks if the given source file belongs to an externally build library.

   function Non_Null_Intersection
     (Left  : File_Array_Access;
      Right : File_Array_Access)
      return Boolean;
   --  Returns True if two file arrays have at least one common file.

   function All_Source_Locations return File_Array_Access;
   --  Returns an array of all directories that may have source files.
   --  In case of project option, returns the list of recursively collected
   --  source dirs, otherwise collects the list from argument source files.

   procedure Get_Naming_Info
     (Source_Project_Tree : GNATCOLL.Projects.Project_Tree);
   --  Gathers all naming info and creates a file with corresponding pragmas.

   procedure Update_Path_With_Project (Dirs : GNATCOLL.VFS.File_Array);
   --  Treats all the source dirs from project as -I option parameters.
   --  Also sets the value of Source_Dirs_Conflict flag.

   procedure Add_Test_Paths;

   procedure Register_Gnattest_Specific_Attributes;

   procedure Get_Gnattest_Specific_Attributes
     (Source_Project_Tree : GNATCOLL.Projects.Project_Tree);

   function Has_Command_Line_Support return Boolean;
   --  Checks whether current run-time library has command line support or not.

   procedure Read_Sources_From_Binder_Output;
   --  Calls the binder for the main unit with '-R' option to get the list of
   --  source making up the main unit closure. If this attempt is successful,
   --  tries to read these sources into the source file table.

   --------------------
   -- Add_Test_Paths --
   --------------------

   procedure Add_Test_Paths is
      Tmp : String_Access;
   begin
      GNATtest.Skeleton.Source_Table.Reset_Source_Iterator;
      loop
         Tmp := new String'(GNATtest.Skeleton.Source_Table.Next_Source_Name);
         if Tmp.all = "" then
            Free (Tmp);
            exit;
         end if;

         Store_I_Option (Get_Source_Output_Dir (Tmp.all));
         Free (Tmp);
      end loop;
   end Add_Test_Paths;

   ------------------------------
   -- Update_Path_With_Project --
   ------------------------------

   procedure Update_Path_With_Project (Dirs : GNATCOLL.VFS.File_Array) is
   begin
      for F in Dirs'Range loop
         Store_I_Option (Dirs (F).Display_Full_Name);
      end loop;
   end Update_Path_With_Project;

   --------------------------
   -- All_Source_Locations --
   --------------------------

   function All_Source_Locations return File_Array_Access is
      Source_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      Tmp         : String_Access;
   begin
      if Source_Prj.all /= ("") then
         --  If we're here, than the project has been successfully loaded.

         Append
           (Source_Dirs,
            Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True));
      else

         GNATtest.Skeleton.Source_Table.Reset_Location_Iterator;

         loop
            Tmp := new String'(Normalize_Pathname
              (Name           =>
                 GNATtest.Skeleton.Source_Table.Next_Source_Location,
               Case_Sensitive => False));
            exit when Tmp.all = "";

            Append (Source_Dirs, GNATCOLL.VFS.Create (+Tmp.all));
         end loop;

      end if;

      return Source_Dirs;
   end All_Source_Locations;

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      Put_Line ("Usage: gnattest -Pprj [opts] {filename} [-cargs switches]");
      Put_Line ("        - generates the unit testing framework");
      New_Line;
      Put_Line ("  or   gnattest test_drivers.list [opts]");
      Put_Line ("        - executes tests and aggregates the results");
      New_Line;

      Put_Line (" --version    Display version and exit");
      Put_Line (" --help       Display usage and exit");
      New_Line;

      Put_Line ("Framework generation mode options:");
      New_Line;
      Put_Line (" -Pprj        Name of the project for which to generate " &
                "the testing framework");
      Put_Line (" {filename}   " &
                "Only generate testing framework for these source files;");
      Put_Line ("              if omitted, consider all project sources");
      New_Line;

      Put_Line (" -q           Quiet mode");
      Put_Line (" -v           Verbose mode");
      Put_Line (" -r           " &
                  "Consider recursively all sources from all projects");
      Put_Line (" -U main      "
                & "Process the closure of units rooted at unit main");
      New_Line;

      Put_Line (" --additional-tests=prj  " &
                "Treat sources from project prj as additional");
      Put_Line ("                         " &
                "manual tests to add to the test suite");
      Put_Line (" --harness-only          " &
                "Treat argument sources as tests to add to the suite");
      New_Line;
      Put_Line (" --separate-drivers[=unit|test]  " &
                  "Generate a separate test driver for each");
      Put_Line ("                                 unit or test, "
                & "unit being the default");
      Put_Line (" --stub                          " &
                  "Generate testing framework that uses stubs");
      New_Line;
      Put_Line (" --exclude-from-stubbing=file      - "
                & "list of sources whose bodies should not");
      Put_Line ("                                     be stubbed");
      Put_Line (" --exclude-from-stubbing:unit=file - "
                & "list of sources whose bodies should not ");
      Put_Line ("                                     be stubbed when"
                & " testung unit");
      New_Line;
      Put_Line (" --harness-dir=dirname   Output dir for test harness");
      Put_Line (" --tests-dir=dirname     Test files are put in dirname");
      Put_Line (" --subdir=dirname        " &
                "Test files are put in subdirs dirname of source dirs");
      Put_Line (" --tests-root=dirname    " &
                "Test files are put in the same directory hierarchy");
      Put_Line ("                         as sources but rooted at dirname");
      Put_Line (" --stubs-dir=dirname     " &
                "Stub files are put in subdirs of dirname");
      New_Line;

      Put_Line (" --validate-type-extensions      " &
                "Run all tests from all parents to check LSP");
      Put_Line (" --skeleton-default=(pass|fail)  " &
                "Default behavior of unimplemented tests");
      Put_Line (" --passed-tests=(show|hide)      " &
                "Default output of passed tests");
      Put_Line (" --exit-status=(on|off)          " &
                "Default usage of the exit status");
      Put_Line (" --omit-sloc                     " &
                "Don't record subprogram sloc in test package");
      Put_Line (" --no-command-line               " &
                "Don't add command line support to test driver");
      Put_Line ("");
      Put_Line (" --test-duration                 " &
                "Show timing for each test");
      New_Line;
      New_Line;

      Put_Line ("Tests execution mode options:");
      New_Line;
      Put_Line (" --passed-tests=(show|hide)      " &
                "Default output of passed tests");
      Put_Line (" --queues=n, -jn                 " &
                "Run n tests in parallel (default n=1)");
   end Brief_Help;

   ----------------------------------
   -- Check_Aggregation_Parameters --
   ----------------------------------

   procedure Check_Aggregation_Parameters is
      Tmp : String_Access;
   begin

      if Source_Buffer.Is_Empty then
         Report_Err ("test driver list not specified");
         raise Parameter_Error;
      end if;

      if Source_Buffer.Length > Count_Type (1) then
         Report_Std
           ("warning: (gnattest) multiple argument files specified,"
            & " only first is processed");
      end if;

      Arg_File_Name := new String'(Source_Buffer.First_Element);

      if Arg_File_Name.all /= "" then
         if not Is_Regular_File (Arg_File_Name.all) then

            if Is_Regular_File (Arg_File_Name.all & ".list") then
               Tmp := new String'(Arg_File_Name.all & ".list");
               Free (Arg_File_Name);
               Arg_File_Name := Tmp;
               Tmp := null;
            else
               Report_Err ("gnattest: test driver list " &
                           Arg_File_Name.all & " does not exist");
               raise Parameter_Error;
            end if;
         end if;
      else
         Report_Err ("test driver list not specified");
         raise Parameter_Error;
      end if;

      Tmp := new String'(Normalize_Pathname
                         (Name           => Arg_File_Name.all,
                          Case_Sensitive => False));
      Free (Arg_File_Name);
      Arg_File_Name := new String'(Tmp.all);
      Free (Tmp);

      Run_Dir := new String'
        (Normalize_Pathname (Name => Get_Current_Dir,
                             Case_Sensitive => False));

      Create_Temp_Dir;
      Change_Dir (Temp_Dir.all);

   end Check_Aggregation_Parameters;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is

      Tmp : String_Access;

      --  Project support:
      Files : File_Array_Access;
      V_F   : Virtual_File;

      Project_Success : Boolean := False;

      procedure Add_AUnit_Paths;
      --  Creates a dummy project file importing aunit, then trying to load it.
      --  If the attempt fails that means that AUnit is not on default project
      --  path.
      --  Sets Options.No_Comand_Line depending of default value of
      --  AUnit_Shared.Runtime.

      procedure Initialize_Environment;

      procedure Set_Gnattest_Generated_Present
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree);
      --  Checks if gnattest_generated.ads is already among sources.

      procedure Set_Ext_Values;
      --  Sets values of external variables passed to gnattest by -X options.
      --  Overwrites the value of Options.No_Comand_Line if RUNTIME is among
      --  scenario variables declared in the project or passed to the tool.

      procedure Set_Inherited_Switches
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree);

      procedure Set_Asis_Mode
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree);

      ---------------------
      -- Add_AUnit_Paths --
      ---------------------

      procedure Add_AUnit_Paths is
         Output_File : File_Type;
         SPT : GNATCOLL.Projects.Project_Tree;
         Firts_Idx : constant Natural := ASIS_UL.Common.Tool_Name'First;
         Last_Idx  : constant Natural :=
           Index (ASIS_UL.Common.Tool_Name.all, "-", Ada.Strings.Backward);
         Local_Env : Project_Environment_Access;
      begin
         GNATCOLL.Traces.Parse_Config_File;
         Initialize (Local_Env);

         if RTS_Attribute_Val = null then
            Local_Env.Set_Target_And_Runtime
              (ASIS_UL.Common.Tool_Name (Firts_Idx .. Last_Idx - 1),
               RTS_Path.all);
         else
            Local_Env.Set_Target_And_Runtime
              (ASIS_UL.Common.Tool_Name (Firts_Idx .. Last_Idx - 1),
               RTS_Attribute_Val.all);
         end if;

         Set_Automatic_Config_File (Local_Env.all);

         Create
           (Output_File,
            Out_File,
            Temp_Dir.all & Directory_Separator & "default.gpr");
         Put_Line (Output_File, "with ""aunit"";");
         Put_Line (Output_File, "project default is");
         Put_Line (Output_File, "end default;");
         Close (Output_File);

         SPT.Load
           (GNATCOLL.VFS.Create
              (+(Temp_Dir.all & Directory_Separator & "default.gpr")),
            Local_Env,
            Report_Missing_Dirs => False);

         Update_Path_With_Project
           (SPT.Root_Project.Source_Dirs (Recursive => True));

         SPT.Unload;

      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Report_Err ("gnattest: aunit not installed");
            raise Parameter_Error;

      end Add_AUnit_Paths;

      procedure Set_Ext_Values is
         Vars : Scenario_Variable_Array :=
           Source_Project_Tree.Scenario_Variables;

         Var_Name, Var_Val : String_Access;

         procedure Supress_Output (Msg : String);
         procedure Supress_Output (Msg : String) is
         begin
            if Quiet then
               return;
            end if;

            if Index (Msg, "warning") /= 0
              and then Index (Msg, "directory") /= 0
              and then Index (Msg, "not found") /= 0
            then
               return;
            else
               Put_Line (Msg);
            end if;
         end Supress_Output;
      begin

         SB_Cur := Ext_Var_Buffer.First;
         loop
            exit when SB_Cur = List_Of_Strings.No_Element;

            declare
               S : constant String := List_Of_Strings.Element (SB_Cur);
               F_Idx : constant Integer := S'First;
               L_Idx : constant Integer := S'Last;
            begin
               for I in S'Range loop
                  if S (I) = '=' then
                     Var_Name := new String'(S (F_Idx .. I - 1));
                     Var_Val  := new String'(S (I + 1 .. L_Idx));
                  end if;
               end loop;

               if Var_Name = null then
                  Report_Std ("-X" & S & " is an illegal option");
                  raise Parameter_Error;
               end if;
            end;

            for I in Vars'Range loop

               if External_Name (Vars (I)) = Var_Name.all then

                  declare
                     Pos_Vals : constant GNAT.Strings.String_List :=
                       Source_Project_Tree.Possible_Values_Of (Vars (I));
                     Present : Boolean := False;
                  begin

                     for J in Pos_Vals'Range loop
                        if Pos_Vals (J).all = Var_Val.all then
                           Present  := True;
                           exit;
                        end if;
                     end loop;

                     if not Present then
                        Report_Err
                          ("value " & Var_Val.all &
                           " is illegal for " & Var_Name.all);
                        raise Parameter_Error;
                     end if;

                  end;

                  Set_Value (Vars (I), Var_Val.all);
                  exit;
               end if;

            end loop;

            List_Of_Strings.Next (SB_Cur);
            Free (Var_Name);
            Free (Var_Val);
         end loop;

         Source_Project_Tree.Change_Environment (Vars);
         Source_Project_Tree.Recompute_View
           (Errors => Supress_Output'Unrestricted_Access);

      end Set_Ext_Values;

      procedure Set_Gnattest_Generated_Present
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree)
      is
         Files : File_Array_Access;
      begin
         Files :=
           Source_Project_Tree.Root_Project.Source_Files (Recursive => True);

         for F in Files'Range loop
            if not Is_Externally_Built (Files (F)) then

               if Files (F).Display_Base_Name = "gnattest_generated.ads" then
                  Gnattest_Generated_Present := True;
                  exit;
               end if;

            end if;
         end loop;

      end Set_Gnattest_Generated_Present;

      procedure Set_Asis_Mode
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree)
      is
         Switches : String_List_Access;
         Proj : constant Project_Type := Source_Project_Tree.Root_Project;
      begin
         if Has_Attribute (Proj, Compiler_Default_Switches_Attribute) then
            Switches :=
              Attribute_Value
              (Proj,
               Compiler_Default_Switches_Attribute,
               "ada");
         end if;

         if Switches = null then
            return;
         end if;

         for I in Switches'Range loop
            if
              Switches (I).all = "-gnat95"
            then
               ASIS_UL.Compiler_Options.Store_Option ("-gnat95");
            end if;
            if
              Switches (I).all = "-gnat2005" or else
              Switches (I).all = "-gnat05"
            then
               ASIS_UL.Compiler_Options.Store_Option ("-gnat05");
            end if;
            if
              Switches (I).all = "-gnat12" or else
              Switches (I).all = "-gnat2012"
            then
               ASIS_UL.Compiler_Options.Store_Option ("-gnat12");
            end if;
            if Index (Switches (I).all, "-gnateD") /= 0 then
               ASIS_UL.Compiler_Options.Store_Option (Switches (I).all);
            end if;
         end loop;
      end Set_Asis_Mode;

      procedure Set_Inherited_Switches
        (Source_Project_Tree : GNATCOLL.Projects.Project_Tree)
      is
         Target_Switches : constant String_List :=
           (new String'("-gnatE"),
            new String'("-gnat12"),
            new String'("-gnat2012"));

         Switches : String_List_Access;

         Proj : constant Project_Type := Source_Project_Tree.Root_Project;
      begin
         if Has_Attribute (Proj, Compiler_Default_Switches_Attribute) then
            Switches :=
              Attribute_Value
              (Proj,
               Compiler_Default_Switches_Attribute,
               "ada");
         end if;

         if Switches = null then
            return;
         end if;

         for I in Switches'Range loop
            for J in Target_Switches'Range loop
               if Switches (I).all = Target_Switches (J).all then
                  Inherited_Switches.Append (Switches (I).all);
               end if;
            end loop;
         end loop;
      end Set_Inherited_Switches;

      procedure Initialize_Environment is
         Firts_Idx : constant Natural := ASIS_UL.Common.Tool_Name'First;
         Last_Idx  : constant Natural :=
           Index (ASIS_UL.Common.Tool_Name.all, "-", Ada.Strings.Backward);

      begin
         GNATCOLL.Traces.Parse_Config_File;
         Initialize (Env);

         Env.Set_Target_And_Runtime
           (ASIS_UL.Common.Tool_Name (Firts_Idx .. Last_Idx - 1),
            RTS_Path.all);

         Set_Automatic_Config_File (Env.all);

      end Initialize_Environment;

   begin
      if Stub_Mode_ON then
         Report_Std
           ("warning: (gnattest) Stubbing is in alpha stage,");
         Report_Std
           (" backwards compatibility cannot be guaranteed "
            & "at this time.");
      end if;

      Register_Gnattest_Specific_Attributes;
      Initialize_Environment;

      if Source_Prj.all /= "" then

         declare
            procedure Errors (S : String);
            procedure Errors (S : String) is
            begin
               if Index (S, " not a regular file") /= 0 then
                  Report_Err ("gnattest: project file "
                              & Source_Prj.all & " does not exist");
               else
                  Report_Err (S);
               end if;
            end Errors;
         begin
            Source_Project_Tree.Load
              (GNATCOLL.VFS.Create (+Source_Prj.all),
               Env,
               Recompute_View      => False,
               Errors              => Errors'Unrestricted_Access);
         exception
               when Invalid_Project =>
                  raise Parameter_Error;
         end;
         Free (Source_Prj);
         Source_Prj := new String'
           (Source_Project_Tree.Root_Project.Project_Path.Display_Full_Name);

         if Is_Aggregate_Project (Source_Project_Tree.Root_Project) then
            Report_Err ("gnattest: aggregate projects are not supported");
            raise Parameter_Error;
         end if;

         Set_Ext_Values;

         Run_Dir := new String'
           (Normalize_Pathname (Name => Get_Current_Dir,
                                Case_Sensitive => False));
         Create_Temp_Dir
           (Source_Project_Tree.Root_Project.Object_Dir.Display_Full_Name);

         if
           RTS_Path.all /= "" or else
           Attribute_Value
             (Source_Project_Tree.Root_Project, Runtime_Attribute, "ada") /= ""
         then
            if Env.Predefined_Object_Path = Empty_File_Array then
               if RTS_Path.all /= "" then
                  Report_Err
                    ("object path not found for runtime " & RTS_Path.all);
               else
                  Report_Err
                    ("object path not found for runtime "
                     & Attribute_Value
                       (Source_Project_Tree.Root_Project,
                        Runtime_Attribute,
                        "ada"));
               end if;
               raise Fatal_Error;
            end if;
            declare
               Files : constant GNATCOLL.VFS.File_Array :=
                 Env.Predefined_Object_Path;
               Obj_Path : constant String :=
                 Files (Files'First).Display_Full_Name;
               Idx : Integer;
            begin
               Idx := Index (Obj_Path, Directory_Separator & "adalib");
               if Idx = 0 then
                  Report_Err ("cannot locate runtime at " & Obj_Path);
                  raise Fatal_Error;
               else
                  ASIS_UL.Compiler_Options.Store_Option
                    ("--RTS="
                     & Obj_Path (Obj_Path'First .. Idx - 1));
                  if RTS_Path.all = "" then
                     --  We might as well update it.
                     Free (RTS_Path);
                     RTS_Path := new String'
                       (Obj_Path (Obj_Path'First .. Idx - 1));
                     RTS_Attribute_Val := new String'
                       (Attribute_Value
                          (Source_Project_Tree.Root_Project,
                           Runtime_Attribute,
                           "ada"));
                  end if;
               end if;
            end;
         end if;

         Update_Path_With_Project
           (Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True));
         Set_Gnattest_Generated_Present (Source_Project_Tree);

         Project_Success := True;
         Get_Gnattest_Specific_Attributes (Source_Project_Tree);

         --  --additional-tests and --harness-only are not yet supported in
         --  --separate-drivers mode.
         --  --stub and --harness-only make no sense at the same time.
         --  --additional-tests is not (yet?) supported in --stub mode.
         if Stub_Mode_ON then
            if Harness_Only then
               Report_Err
                 ("options --harness-only and --stub are incompatible");
               raise Parameter_Error;
            end if;
            if Additional_Tests_Prj /= null then
               Report_Err
                 ("options --additional-tests and --stub are incompatible");
               raise Parameter_Error;
            end if;

            --  We also need to change default dirs is they have not been
            --  changed explicitly.
            if not Tests_Dir_Set then
               Free (Test_Dir_Name);
               Test_Dir_Name := new String'
                 ("gnattest_stub" & Directory_Separator & "tests");
            end if;
            if not Stub_Dir_Set then
               Free (Stub_Dir_Name);
               Stub_Dir_Name := new String'
                 ("gnattest_stub" & Directory_Separator & "stubs");
            end if;
            if not Harness_Dir_Set then
               Free (Harness_Dir);
               Harness_Dir := new String'
                 ("gnattest_stub" & Directory_Separator & "harness");
            end if;
         end if;
         if Separate_Drivers then
            if Harness_Only then
               Report_Err
                 ("options --harness-only and --separate-drivers "
                  & "are incompatible");
               raise Parameter_Error;
            end if;
            if Additional_Tests_Prj /= null then
               Report_Err
                 ("options --additional-tests and --separate-drivers "
                  & "are incompatible");
               raise Parameter_Error;
            end if;
         end if;
         Add_AUnit_Paths;

         --  Checking if argument project has IDE package specified.
         declare
            S : constant Attribute_Pkg_String := Build (Ide_Package, "");
         begin
            if Has_Attribute (Source_Project_Tree.Root_Project, S) then
               IDE_Package_Present := True;
            else
               IDE_Package_Present := False;
            end if;
         end;

         --  Checking if argument project has Make package specified.
         declare
            S : constant Attribute_Pkg_String := Build ("make", "");
         begin
            if Has_Attribute (Source_Project_Tree.Root_Project, S) then
               Make_Package_Present := True;
            else
               Make_Package_Present := False;
            end if;
         end;

         if Stub_Mode_ON then
            GNATtest.Skeleton.Source_Table.Initialize_Project_Table;
         end if;

         Object_Dir := new String'
           (Source_Project_Tree.Root_Project.Object_Dir.Display_Full_Name);

         if Output_M = Separate_Root then
            if Separate_Root_Dir.all = Harness_Dir.all then
               Report_Err
                 ("harness dir and separate root dir should not be the same");
               raise Parameter_Error;
            end if;
         end if;

      else
         Report_Err ("gnattest: project file not specified");
         raise Parameter_Error;
      end if;

      No_Command_Line := not Has_Command_Line_Support;

      --  need to check if given file is a source file of argument project
      Files := new File_Array'
        (Source_Project_Tree.Root_Project.Source_Dirs (True));

      SB_Cur := Source_Buffer.First;
      loop

         exit when SB_Cur = List_Of_Strings.No_Element;

         V_F := Locate_Regular_File
           (+(Base_Name (List_Of_Strings.Element (SB_Cur))),
            Files.all);

         if V_F = No_File then
            Report_Err
              (Base_Name (List_Of_Strings.Element (SB_Cur))
               & " is not part of "
               & Base_Name (Source_Prj.all)
               & " or its dependencies");
         else

            if Harness_Only then
               GNATtest.Harness.Source_Table.Add_Source_To_Process
                 (List_Of_Strings.Element (SB_Cur));
            else
               GNATtest.Skeleton.Source_Table.Add_Source_To_Process
                 (List_Of_Strings.Element (SB_Cur));
            end if;

         end if;

         List_Of_Strings.Next (SB_Cur);
      end loop;

      Source_Buffer.Clear;

      if Harness_Dir /= null then

         if not Is_Absolute_Path (GNATCOLL.VFS.Create (+Harness_Dir.all)) then
            Tmp := new String'(Object_Dir.all & Harness_Dir.all);
            Free (Harness_Dir);
            Harness_Dir := Tmp;
            Tmp := null;
         end if;

         if Is_Regular_File (Harness_Dir.all) then
            Report_Err ("gnattest: cannot create harness directory");
            raise Parameter_Error;
         elsif not Is_Directory (Harness_Dir.all) then

            declare
               Dir : File_Array_Access;
            begin
               Append (Dir, GNATCOLL.VFS.Create (+Harness_Dir.all));
               Create_Dirs (Dir);
            exception
               when Directory_Error =>
                  Report_Err ("gnattest: cannot create harness directory");
                  raise Parameter_Error;
            end;

         end if;

      else
         Report_Err ("gnattest: harness directory not specified");
         raise Parameter_Error;
      end if;

      Tmp := new String'(Normalize_Pathname
        (Name           => Harness_Dir.all,
         Case_Sensitive => False));
      Free (Harness_Dir);
      Harness_Dir := new String'(Tmp.all & Directory_Separator);
      Free (Tmp);

      Change_Dir (Temp_Dir.all);
      if Project_Success then
         Get_Naming_Info (Source_Project_Tree);
         Set_Inherited_Switches (Source_Project_Tree);
         Set_Asis_Mode (Source_Project_Tree);
      end if;

      if Main_Unit /= null then
         Read_Sources_From_Binder_Output;
      end if;

      if Harness_Only then
         if GNATtest.Harness.Source_Table.SF_Table_Empty then

            if Source_Prj.all = "" then

               Report_Err ("No input source file set");
               raise Parameter_Error;

            else

               Files :=
                 Source_Project_Tree.Root_Project.Source_Files
                   (Recursive => False);

               --  If Files is still empty, that means that the given
               --  project does not have any source files.
               if Files'Length = 0 then
                  Report_Err
                    (Source_Prj.all & " doesn't contain source files");
                  raise Parameter_Error;
               end if;

               for F in Files'Range loop
                  if not Is_Externally_Built (Files (F)) then

                     if
                       Source_Project_Tree.Info (Files (F)).Unit_Part =
                       Unit_Spec
                     then
                        GNATtest.Harness.Source_Table.Add_Source_To_Process
                          (Files (F).Display_Full_Name);
                     end if;
                  end if;
               end loop;

               --  If SF_Table is still empty, that means that the given
               --  project does not have any testable source files.
               if GNATtest.Harness.Source_Table.SF_Table_Empty then
                  Report_Err
                    (Source_Prj.all &
                     " doesn't contain testable source files");
                  raise Parameter_Error;
               end if;

            end if;

         end if;
      else

         if GNATtest.Skeleton.Source_Table.SF_Table_Empty then
            if Source_Prj.all = "" then
               Report_Err ("No input source file set");
               raise Parameter_Error;

            else
               declare
                  P : Project_Type := Source_Project_Tree.Root_Project;
               begin
                  Files := P.Source_Files
                   (Recursive => Recursive_Sources);
                  if not Recursive_Sources then
                     --  We need to add all sources from extended projects.
                     loop
                        P := Extended_Project (P);
                        exit when P = No_Project;

                        Append (Files, P.Source_Files.all);
                     end loop;
                  end if;
               end;

               --  If Files is still empty, that means that the given
               --  project does not have any source files.
               if Files'Length = 0 then
                  Report_Err
                    (Source_Prj.all & " doesn't contain source files");
                  raise Parameter_Error;
               end if;

               for F in Files'Range loop
                  if not Is_Externally_Built (Files (F)) then

                     if
                       Source_Project_Tree.Info (Files (F)).Unit_Part =
                       Unit_Spec
                     then
                        GNATtest.Skeleton.Source_Table.Add_Source_To_Process
                          (Files (F).Display_Full_Name);
                     end if;

                  end if;
               end loop;

               --  If SF_Table is still empty, that means that the given
               --  project does not have any testable source files.
               if GNATtest.Skeleton.Source_Table.SF_Table_Empty then
                  Report_Err
                    (Source_Prj.all &
                     " doesn't contain testable source files");
                  raise Parameter_Error;
               end if;

            end if;
         end if;

         if Stub_Mode_ON then
            Files := Source_Project_Tree.Root_Project.Source_Files
              (Recursive => True);

            for F in Files'Range loop
               case Source_Project_Tree.Info (Files (F)).Unit_Part is
                  when Unit_Body =>
                     declare
                        P : Project_Type :=
                          Source_Project_Tree.Info (Files (F)).Project;
                     begin
                        --  The name of the project here will be used to create
                        --  stub projects. Those extend original projects, so
                        --  is a source belongs to an extended project we need
                        --  the extending on here instead, so that we do not
                        --  end up with different extensions of same project.
                        while Extending_Project (P) /= No_Project loop
                           P := Extending_Project (P);
                        end loop;

                        GNATtest.Skeleton.Source_Table.Add_Body_To_Process
                          (Files (F).Display_Full_Name, P.Name);
                     end;
                  when Unit_Spec =>
                     GNATtest.Skeleton.Source_Table.Add_Body_Reference
                       (Files (F).Display_Full_Name);
                  when others =>
                     null;
               end case;
            end loop;
         end if;

         case Output_M is
            when Subdir =>
               Check_Subdir;
            when Parallel =>
               Check_Parallel;
            when Separate_Root =>
               Check_Separate_Root;
            when Direct =>
               Check_Direct;
         end case;

         if Stub_Mode_ON then
            Check_Stub;
            Process_Exclusion_Lists;
         end if;
      end if;

      Add_Test_Paths;

      if Additional_Tests_Prj /= null then

         Change_Dir (Run_Dir.all);
         Tmp := new String'(Normalize_Pathname
           (Name           => Additional_Tests_Prj.all,
            Case_Sensitive => False));
         Free (Additional_Tests_Prj);
         Additional_Tests_Prj := new String'(Tmp.all);
         Free (Tmp);
         Change_Dir (Temp_Dir.all);

         if not Is_Regular_File (Additional_Tests_Prj.all) then
            if Is_Regular_File (Additional_Tests_Prj.all & ".gpr") then
               Tmp := new String'(Additional_Tests_Prj.all & ".gpr");
               Free (Additional_Tests_Prj);
               Additional_Tests_Prj := Tmp;
               Tmp := null;
            else
               Report_Err ("gnattest: project file " &
                           Additional_Tests_Prj.all & " does not exist");
               raise Parameter_Error;
            end if;
         end if;

         Source_Project_Tree.Load
           (GNATCOLL.VFS.Create (+Additional_Tests_Prj.all), Env);
         Update_Path_With_Project
           (Source_Project_Tree.Root_Project.Source_Dirs (Recursive => True));

         Set_Gnattest_Generated_Present (Source_Project_Tree);

         Files := Source_Project_Tree.Root_Project.Source_Files
           (Recursive => False);

         for F in Files'Range loop
            if not Is_Externally_Built (Files (F)) then

               if
                 Source_Project_Tree.Info (Files (F)).Unit_Part =
                 Unit_Spec
               then
                  GNATtest.Harness.Source_Table.Add_Source_To_Process
                    (Files (F).Display_Full_Name);
               end if;
            end if;
         end loop;

      end if;

      if not Harness_Only then
         Store_I_Option (Harness_Dir.all & Directory_Separator & "common");
      end if;

      --  Disregard the fact that Process_cargs_Section calls Set_Arg_List and
      --  Process_ADA_PRJ_INCLUDE_FILE already, whithout their explicit call
      --  compiltaion dependancies for sources from imported projects are not
      --  resolved.
      --  Thay cannot be replaced with Process_cargs_Section here.
      Process_ADA_PRJ_INCLUDE_FILE;
      Store_I_Options;
      Set_Arg_List;

      GNATCOLL.Projects.Aux.Delete_All_Temp_Files
        (Source_Project_Tree.Root_Project);
      Source_Project_Tree.Unload;
      Free (Env);
   end Check_Parameters;

   ------------------
   -- Check_Direct --
   ------------------

   procedure Check_Direct is
      Tmp : String_Access;
      TD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Test_Dir_Name.all);
      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);

      Obj_Dir : String_Access;

      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start (Source_Project_Tree.Root_Project);
   begin

      if TD_Name.Is_Absolute_Path then
         Append (Future_Dirs, GNATCOLL.VFS.Create (+Test_Dir_Name.all));
      else
         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);
            Tmp := new String'(Obj_Dir.all & Test_Dir_Name.all);
            Append (Future_Dirs, GNATCOLL.VFS.Create (+Tmp.all));
            Free (Tmp);
            Free (Obj_Dir);

            Next (Iterator);
         end loop;
      end if;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Report_Err ("gnattest: invalid output directory, cannot mix up " &
                     "tests and infrastructure");
         raise Parameter_Error;
      end if;

      Set_Direct_Output;
   end Check_Direct;

   ----------------
   -- Check_Stub --
   ----------------

   procedure Check_Stub is
      Tmp : String_Access;
      SD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Stub_Dir_Name.all);
      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);

      Obj_Dir : String_Access;

      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start (Source_Project_Tree.Root_Project);
   begin

      --  look for collisions with source dirs
      if SD_Name.Is_Absolute_Path then
         Append (Future_Dirs, GNATCOLL.VFS.Create (+Test_Dir_Name.all));
      else
         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);
            Tmp := new String'(Obj_Dir.all & Stub_Dir_Name.all);
            Append (Future_Dirs, GNATCOLL.VFS.Create (+Tmp.all));
            Free (Tmp);
            Free (Obj_Dir);

            Next (Iterator);
         end loop;
      end if;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Report_Err ("gnattest: invalid stub directory, cannot mix up "
                     & "stubs and source files");
         raise Parameter_Error;
      end if;

      Set_Direct_Stub_Output;

      --  Once stub dirs are set we can compare them with test dirs per source.
      Skeleton.Source_Table.Reset_Source_Iterator;
      Tmp := new String'(Skeleton.Source_Table.Next_Source_Name);
      while Tmp.all /= "" loop
         if
           Skeleton.Source_Table.Get_Source_Output_Dir (Tmp.all) =
           Skeleton.Source_Table.Get_Source_Stub_Dir (Tmp.all)
         then
            Report_Err ("gnattest: invalid stub directory, cannot mix up "
                        & "stubs and tests");
            raise Parameter_Error;
         end if;
         Free (Tmp);
         Tmp := new String'(Skeleton.Source_Table.Next_Source_Name);
      end loop;

      Skeleton.Source_Table.Reset_Source_Iterator;
   end Check_Stub;

   --------------------
   -- Check_Parallel --
   --------------------

   procedure Check_Parallel is
      Tmp             : String_Access;
      Base_Dir_Name   : String_Access;
      Target_Dir_Name : String_Access;

      Idx         : Integer;

      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      --  List of dirs to be generated. The list is checked for intersections
      --  with source dirs before any new directories are created.

   begin
      GNATtest.Skeleton.Source_Table.Reset_Location_Iterator;

      loop
         Tmp := new String'
           (GNATtest.Skeleton.Source_Table.Next_Source_Location);
         exit when Tmp.all = "";

         if Tmp.all (Tmp.all'Last) = Directory_Separator then
            Idx := Tmp.all'Last - 1;
         else
            Idx := Tmp.all'Last;
         end if;

         for I in reverse Tmp.all'First .. Idx loop
            if Tmp.all (I) = Directory_Separator then
               Base_Dir_Name := new String'(Tmp.all (Tmp.all'First .. I - 1));
               Target_Dir_Name := new String'(Tmp.all (I + 1 .. Tmp.all'Last));
               exit;
            end if;
         end loop;

         if Base_Dir_Name = null then
            Report_Err ("gnattest: sources in root directory," &
                        " cannot make parallel dirs");
            raise Parameter_Error;
         end if;

         Append (Future_Dirs, GNATCOLL.VFS.Create
           (+(Base_Dir_Name.all   &
              Directory_Separator &
              Test_Dir_Prefix.all &
              Target_Dir_Name.all &
              Test_Dir_Suffix.all)));
      end loop;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Report_Err ("gnattest: invalid output directory, cannot mix up " &
                     "tests and infrastructure");
         raise Parameter_Error;
      end if;

      Set_Parallel_Output;
   end Check_Parallel;

   -------------------------
   -- Check_Separate_Root --
   -------------------------

   procedure Check_Separate_Root is

      RD_Name : constant Virtual_File :=
        GNATCOLL.VFS.Create (+Separate_Root_Dir.all);

      Tmp, Buff    : String_Access;
      Maximin_Root : String_Access;
      Root_Length  : Integer;

      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      --  List of dirs to be generated. The list is checked for intersections
      --  with source dirs before any new directories are created.

      Files : File_Array_Access;
      Project  : Project_Type;
      Iterator : Project_Iterator :=
        Start_Reversed (Source_Project_Tree.Root_Project,
               Recursive => Recursive_Sources);

      Ext_Bld : constant Attribute_Pkg_String :=
        Build ("", "externally_built");

      Obj_Dir                 : String_Access;
      Local_Separate_Root_Dir : String_Access;

      function Common_Root (Left : String; Right : String) return String;
      --  Returns the coincident beginning of both paths or an empty string.

      -------------------
      --  Common_Root  --
      -------------------

      function Common_Root (Left : String; Right : String) return String is
         Idxl : Integer := Left'First;
         Idxr : Integer := Right'First;

         Last_Dir_Sep_Index : Integer := Idxl - 1;
         --  We need to check for the following:
         --  ...somepath/dir/
         --  ...somepath/directory/

      begin
         if Left = "" or Right = "" then
            return "";
         end if;

         loop
            if Left (Idxl) = Directory_Separator
              and then Right (Idxr) = Directory_Separator
            then
               Last_Dir_Sep_Index := Idxl;
            end if;

            if Left (Idxl) /= Right (Idxr) then
               return Left (Left'First .. Last_Dir_Sep_Index);
            end if;

            exit when Idxl = Left'Last or Idxr = Right'Last;

            Idxl := Idxl + 1;
            Idxr := Idxr + 1;
         end loop;

         return Left (Left'First .. Idxl);
      end Common_Root;

   begin

      if RD_Name.Is_Absolute_Path then

         GNATtest.Skeleton.Source_Table.Reset_Location_Iterator;
         Tmp := new String'
           (GNATtest.Skeleton.Source_Table.Next_Source_Location);
         Maximin_Root := new String'(Tmp.all);

         loop
            Tmp := new String'
              (GNATtest.Skeleton.Source_Table.Next_Source_Location);
            exit when Tmp.all = "";

            Buff := new String'(Common_Root (Tmp.all, Maximin_Root.all));

            if Buff.all = "" then
               Report_Err ("gnattest: sources have different root dirs, " &
                           "cannot apply separate root output");
               raise Parameter_Error;
            end if;

            Free (Maximin_Root);
            Maximin_Root := new String'(Buff.all);
            Free (Buff);
            Free (Tmp);
         end loop;

         Root_Length := Maximin_Root.all'Length;

         Separate_Root_Dir := new String'
           (Normalize_Pathname (Name => Separate_Root_Dir.all,
                                Case_Sensitive => False));

         GNATtest.Skeleton.Source_Table.Reset_Location_Iterator;

         loop
            Tmp := new String'
              (GNATtest.Skeleton.Source_Table.Next_Source_Location);
            exit when Tmp.all = "";

            Append (Future_Dirs, GNATCOLL.VFS.Create
              (+(Separate_Root_Dir.all & Directory_Separator &
                 Tmp.all (Root_Length + 1 .. Tmp.all'Last))));

            Free (Tmp);
         end loop;

         if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
            Report_Err ("gnattest: invalid output directory, cannot mix up " &
                        "tests and infrastructure");
            raise Parameter_Error;
         end if;

         Set_Separate_Root (Maximin_Root.all);
      else

         loop
            Project := Current (Iterator);
            exit when Project = No_Project;

            declare
               Dirs : constant File_Array := Project.Source_Dirs (False);

               Common_Root_Dir : String_Access;
            begin
               if Dirs'Length > 0 then
                  Common_Root_Dir := new String'
                    (Dirs (Dirs'First).Display_Full_Name);

                  for J in Dirs'Range loop
                     Tmp := new String'(Dirs (J).Display_Full_Name);
                     Buff := new String'
                       (Common_Root (Tmp.all, Common_Root_Dir.all));

                     if Buff.all = "" then
                        Report_Err
                          ("gnattest: sources have different root dirs, " &
                             "cannot apply separate root output");
                        raise Parameter_Error;
                     end if;

                     Free (Common_Root_Dir);
                     Common_Root_Dir := new String'(Buff.all);
                     Free (Buff);
                     Free (Tmp);
                  end loop;

                  for J in Dirs'Range loop
                     if Dirs (J).Display_Full_Name = Common_Root_Dir.all then
                        Maximin_Root := Common_Root_Dir;
                        exit;
                     end if;
                  end loop;
               end if;
            end;

            Files := Project.Source_Files;

            if Files'Length > 0 then
               if Maximin_Root = null then
                  Maximin_Root := new String'
                    (Files (Files'First).Display_Dir_Name);
               end if;

               for F in Files'Range loop
                  Tmp := new String'(Files (F).Display_Dir_Name);
                  Buff := new String'(Common_Root (Tmp.all, Maximin_Root.all));

                  if Buff.all = "" then
                     Report_Err
                       ("gnattest: sources have different root dirs, " &
                        "cannot apply separate root output");
                     raise Parameter_Error;
                  end if;

                  Free (Maximin_Root);
                  Maximin_Root := new String'(Buff.all);
                  Free (Buff);
                  Free (Tmp);
               end loop;

               Root_Length := Maximin_Root.all'Length;

               Obj_Dir := new String'(Project.Object_Dir.Display_Full_Name);

               Local_Separate_Root_Dir := new String'
                 (Normalize_Pathname
                    (Name => Obj_Dir.all & Separate_Root_Dir.all,
                     Case_Sensitive => False));

               for F in Files'Range loop

                  if
                    Source_Project_Tree.Info (Files (F)).Unit_Part = Unit_Spec
                  then
                     Tmp := new String'(Files (F).Display_Dir_Name);

                     Append (Future_Dirs, GNATCOLL.VFS.Create
                       (+(Local_Separate_Root_Dir.all & Directory_Separator &
                          Tmp.all (Root_Length + 1 .. Tmp.all'Last))));

                     Set_Output_Dir
                       (Files (F).Display_Full_Name,
                        Local_Separate_Root_Dir.all & Directory_Separator &
                        Tmp.all (Root_Length + 1 .. Tmp.all'Last));
                  end if;

               end loop;

            end if;

            --  Externally built projects should be skipped.
            loop
               Next (Iterator);

               if
                 Current (Iterator) = No_Project
                 or else (not Has_Attribute (Current (Iterator), Ext_Bld))
                 or else
                   To_Lower
                     (Attribute_Value (Current (Iterator), Ext_Bld)) /= "true"
               then
                  exit;
               end if;
            end loop;
         end loop;

         if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
            Report_Err ("gnattest: invalid output directory, cannot mix up " &
                        "tests and infrastructure");
            raise Parameter_Error;
         end if;

      end if;

   end Check_Separate_Root;

   ------------------
   -- Check_Subdir --
   ------------------

   procedure Check_Subdir is
      Tmp : String_Access;

      Future_Dirs : File_Array_Access := new File_Array'(Empty_File_Array);
      --  List of dirs to be generated. The list is checked for intersections
      --  with source dirs before any new directories are created.
   begin
      GNATtest.Skeleton.Source_Table.Reset_Location_Iterator;

      loop
         Tmp := new String'
           (GNATtest.Skeleton.Source_Table.Next_Source_Location);
         exit when Tmp.all = "";

         Append (Future_Dirs, GNATCOLL.VFS.Create
                 (+(Tmp.all & Directory_Separator & Test_Subdir_Name.all)));
      end loop;

      if Non_Null_Intersection (Future_Dirs, All_Source_Locations) then
         Report_Err ("gnattest: invalid output directory, cannot mix up " &
                     "tests and infrastructure");
         raise Parameter_Error;
      end if;

      Set_Subdir_Output;
   end Check_Subdir;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin
      Context_Clean_Up;

      --  Cleaning up temporary dir

      if Temp_Dir /= null then
         Change_Dir (Run_Dir.all);

         begin
            Remove_Dir (Temp_Dir.all, Recursive => True);
         exception
            when Directory_Error =>
               Free (Temp_Dir);  -- to avoid cycling
               Report_Err ("gnattest: cannot remove temporary directory");
               raise Fatal_Error;
         end;

         Free (Temp_Dir);
      end if;
   end Clean_Up;

   ----------------------
   -- Context_Clean_Up --
   ----------------------

   procedure Context_Clean_Up is
   begin
      if Is_Open (The_Context) then
         Close (The_Context);
      end if;

      if Has_Associations (The_Context) then
         Dissociate (The_Context);
      end if;
   end Context_Clean_Up;

   ---------------------
   -- Create_Temp_Dir --
   ---------------------

   procedure Create_Temp_Dir (Obj_Dir : String := "") is
      FD        : File_Descriptor;
      Temp_Name : Temp_File_Name;
      Success   : Boolean;

      CD : constant String :=
        Normalize_Pathname (Name => Get_Current_Dir,
                            Case_Sensitive => False);
   begin
      --  Here we use exactly the same approach as in gnatelim

      --  ??? We create the temp dir by first creating the temp file, then
      --  closing and deleting it, then creating a dir with the same name.
      --  This is not atomary as another program can sneak in between file
      --  deletion and dir creation and snatch this name for itself. This is
      --  quite unlikely and anyway we don't have any other system-independent
      --  way at the moment
      if Obj_Dir /= "" then
         declare
            F : File_Array_Access;
         begin
            Append (F, Dir (GNATCOLL.VFS.Create (+(Obj_Dir))));
            Create_Dirs (F);
         end;
         Change_Dir (Obj_Dir);
      end if;
      Create_Temp_File (FD, Temp_Name);
      Close (FD);
      Delete_File (Temp_Name, Success);

      if not Success then
         Report_Err ("gnattest: cannot delete the temporary file that was "
                     & "just created");

         raise Fatal_Error;
      end if;

      Make_Dir (Temp_Name);

      Temp_Dir :=
        new String'
          (Obj_Dir & Temp_Name (Temp_Name'First .. Temp_Name'Last - 1));

      Change_Dir (CD);
   exception
      when Directory_Error =>
         Report_Err ("gnattest: cannot create the temporary directory");
         raise Fatal_Error;
   end Create_Temp_Dir;

   --------------------------------------
   -- Get_Gnattest_Specific_Attributes --
   --------------------------------------

   procedure Get_Gnattest_Specific_Attributes
     (Source_Project_Tree : GNATCOLL.Projects.Project_Tree)
   is
      Proj : constant Project_Type := Source_Project_Tree.Root_Project;
      GT_Package : constant String := "gnattest";

   begin

      declare
         Attr : constant Attribute_Pkg_String :=
           Build (GT_Package, "harness_dir");
      begin
         if Attribute_Value (Proj, Attr) /= "" then
            if not Harness_Set_By_Switch then
               Harness_Dir := new String'(Attribute_Value (Proj, Attr));
               Harness_Dir_Set := True;
            end if;
         end if;
      end;

      if not Output_M_Set then

         declare
            Attr1 : constant Attribute_Pkg_String :=
              Build (GT_Package, "subdir");
            Attr2 : constant Attribute_Pkg_String :=
              Build (GT_Package, "tests_root");
            Attr3 : constant Attribute_Pkg_String :=
              Build (GT_Package, "tests_dir");

            Output_Attr_Counter : Natural := 0;
         begin
            if Attribute_Value (Proj, Attr1) /= "" then
               Output_Attr_Counter := Output_Attr_Counter + 1;
            end if;
            if Attribute_Value (Proj, Attr2) /= "" then
               Output_Attr_Counter := Output_Attr_Counter + 1;
            end if;
            if Attribute_Value (Proj, Attr3) /= "" then
               Output_Attr_Counter := Output_Attr_Counter + 1;
            end if;

            if Output_Attr_Counter > 1 then
               Report_Err
                 ("invalid project file: " &
                  "more than one output mode specified");
               raise Fatal_Error;
            end if;
            if Attribute_Value (Proj, Attr1) /= "" then
               Output_M := Subdir;
               Test_Subdir_Name := new String'(Attribute_Value (Proj, Attr1));
               Tests_Dir_Set := True;
            end if;
            if Attribute_Value (Proj, Attr2) /= "" then
               Output_M := Separate_Root;
               Separate_Root_Dir := new String'(Attribute_Value (Proj, Attr2));
               Tests_Dir_Set := True;
            end if;
            if Attribute_Value (Proj, Attr3) /= "" then
               Output_M := Direct;
               Test_Dir_Name := new String'(Attribute_Value (Proj, Attr3));
               Tests_Dir_Set := True;
            end if;
         end;

      end if;

      if Additional_Tests_Prj = null then
         declare
            Attr : constant Attribute_Pkg_String :=
              Build (GT_Package, "additional_tests");
         begin
            if Attribute_Value (Proj, Attr) /= "" then
               Additional_Tests_Prj := new String'
                 (Attribute_Value (Proj, Attr));
            end if;
         end;
      end if;

      if not Default_Skeletons_Set then
         declare
            Attr : constant Attribute_Pkg_String :=
              Build (GT_Package, "skeletons_default");
         begin
            if Attribute_Value (Proj, Attr) /= "" then
               if Attribute_Value (Proj, Attr) = "pass" then
                  Skeletons_Fail := False;
               elsif Attribute_Value (Proj, Attr) = "fail" then
                  Skeletons_Fail := True;
               else
                  Report_Err
                    ("gnattest: skeletons_default " &
                     "should be either fail or pass");
                  raise Parameter_Error;
               end if;
            end if;
         end;
      end if;

      declare
         Attr : constant Attribute_Pkg_List :=
           Build (GT_Package, "gnattest_switches");
         GT_Switches : String_List_Access;
      begin
         if Has_Attribute (Proj, Attr) then
            GT_Switches := Attribute_Value (Proj, Attr);
            for I in GT_Switches'Range loop
               if GT_Switches (I).all = "-r" then
                  Recursive_Sources := True;
               end if;
               if GT_Switches (I).all = "-q" then
                  Quiet := True;
               end if;
               if GT_Switches (I).all = "--validate-type-extensions" then
                  Substitution_Suite := True;
               end if;
               if GT_Switches (I).all = "--harness-only" then
                  Harness_Only := True;
               end if;
               if GT_Switches (I).all = "-v" then
                  if not Verbose then
                     Verbose := True;
                     Print_Version_Info (2011);
                  end if;
               end if;
               if GT_Switches (I).all = "--omit-sloc" then
                  Omit_Sloc := True;
               end if;
               if GT_Switches (I).all = "--test-duration" then
                  Omit_Sloc := True;
               end if;
               if not Show_Passed_Tests_Set then
                  if GT_Switches (I).all = "--passed-tests=show" then
                     Show_Passed_Tests := True;
                  end if;
                  if GT_Switches (I).all = "--passed-tests=hide" then
                     Show_Passed_Tests := False;
                  end if;
               end if;
               if not Add_Exit_Status_Set then
                  if GT_Switches (I).all = "--exit_status=on" then
                     Add_Exit_Status := True;
                  end if;
                  if GT_Switches (I).all = "--passed-tests=off" then
                     Add_Exit_Status := False;
                  end if;
               end if;
               if not Separate_Drivers_Set_By_Switch then
                  if GT_Switches (I).all = "--separate-drivers" or else
                    GT_Switches (I).all = "--separate-drivers=unit"
                  then
                     Separate_Drivers := True;
                     Driver_Per_Unit := True;
                  end if;
                  if GT_Switches (I).all = "--separate-drivers=test" then
                     Separate_Drivers := False;
                     Driver_Per_Unit := False;
                  end if;
               end if;
            end loop;
         end if;
      end;

      if Default_Stub_Exclusion_List_File = null then
         declare
            Attr : constant Attribute_Pkg_String :=
              Build (GT_Package, "default_stub_exclusion_list");
         begin
            if Attribute_Value (Proj, Attr) /= "" then
               Default_Stub_Exclusion_List_File := new String'
                 (Attribute_Value (Proj, Attr));
            end if;
         end;
      end if;

      declare
         Attr    : constant Attribute_Pkg_String :=
           Build (GT_Package, "stub_exclusion_list");
         Indexes : constant String_List          :=
           Attribute_Indexes (Proj, Attr);
      begin
         for I in Indexes'Range loop
            if not Stub_Exclusion_List_Files.Contains (Indexes (I).all) then
               Stub_Exclusion_List_Files.Include
                 (Indexes (I).all,
                  Attribute_Value (Proj, Attr, Indexes (I).all));
            end if;
         end loop;
      end;

   end Get_Gnattest_Specific_Attributes;

   ---------------------
   -- Get_Naming_Info --
   ---------------------

   procedure Get_Naming_Info
     (Source_Project_Tree : GNATCOLL.Projects.Project_Tree)
   is
      Project : Project_Type;
      Iterator        : Project_Iterator :=
        Start (Source_Project_Tree.Root_Project);

      Output_File : File_Type;

      Sources : constant File_Array_Access :=
        Source_Project_Tree.Root_Project.Source_Files (Recursive => True);

      package Specific_Sources_Set is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Specific_Sources_Set;

      Specific_Sources : Specific_Sources_Set.Set;
      Cur : Specific_Sources_Set.Cursor;

      function File_To_Unit
        (File : String; Suff : String; Dot : String) return String;

      function File_To_Unit
        (File : String; Suff : String; Dot : String) return String
      is
         Name : constant String :=
           File (File'First .. File'Last - Suff'Length);
         Idx        :          Integer := Name'First;
         Dot_Length : constant Integer := Dot'Length;

         Tmp : String_Access;
         Res : String_Access := new String'("");
      begin

         for I in Name'Range loop

            if I + Dot_Length <= Name'Last then
               if Name (I .. I + Dot_Length) = Dot then

                  if Res.all = "" then
                     Free (Res);
                     Res := new String'(Name (Idx .. I - 1));
                  else
                     Tmp := new String'(Res.all & "." & Name (Idx .. I - 1));
                     Free (Res);
                     Res := Tmp;
                     Tmp := null;
                  end if;
                  Idx := Idx + Dot_Length;

               end if;
            end if;

            if I = Name'Last then
               Tmp := new String'(Res.all & Name (Idx .. I));
               Free (Res);
               Res := Tmp;
               Tmp := null;
            end if;

         end loop;

         return Res.all;
      end File_To_Unit;

   begin
      Create (Output_File, Out_File, Config_Name);

      loop
         Project := Current (Iterator);
         exit when Project = No_Project;

         if Has_Attribute (Project, Spec_Attribute) then
            declare
               Atr_Indexes : constant GNAT.Strings.String_List :=
                 Attribute_Indexes (Project, Spec_Attribute);
            begin
               for I in Atr_Indexes'Range loop
                  Put_Line
                    (Output_File,
                     "pragma Source_File_Name (" &
                     Atr_Indexes (I).all         &
                     ",");
                  Put_Line
                    (Output_File,
                     "Spec_File_Name => """  &
                     Attribute_Value
                       (Project,
                        Spec_Attribute,
                        Atr_Indexes (I).all) &
                     """);");
                  Specific_Sources.Include
                    (Attribute_Value
                       (Project,
                        Spec_Attribute,
                        Atr_Indexes (I).all));
               end loop;
            end;
         end if;

         if Has_Attribute (Project, Body_Attribute) then
            declare
               Atr_Indexes : constant GNAT.Strings.String_List :=
                 Attribute_Indexes (Project, Body_Attribute);
            begin
               for I in Atr_Indexes'Range loop
                  Put_Line
                    (Output_File,
                     "pragma Source_File_Name (" &
                     Atr_Indexes (I).all         &
                     ",");
                  Put_Line
                    (Output_File,
                     "Body_File_Name => """  &
                     Attribute_Value
                       (Project,
                        Body_Attribute,
                        Atr_Indexes (I).all) &
                     """);");
                  Specific_Sources.Include
                    (Attribute_Value
                       (Project,
                        Body_Attribute,
                        Atr_Indexes (I).all));
               end loop;
            end;
         end if;

         Next (Iterator);
      end loop;

      for F in Sources'Range loop

         if
           Source_Project_Tree.Info (Sources (F)).Unit_Part = Unit_Spec
         then
            Cur := Specific_Sources.Find (Sources (F).Display_Base_Name);

            if Cur = Specific_Sources_Set.No_Element then

               Project :=
                 GNATCOLL.Projects.Project
                   (Source_Project_Tree.Info (Sources (F)));
               declare
                  Suffix  : constant String :=
                    Attribute_Value (Project, Spec_Suffix_Attribute, "ada");
                  Dot_Rep : constant String :=
                    Attribute_Value
                      (Project, Dot_Replacement_Attribute, "ada");
               begin
                  if Suffix /= ".ads" or else Dot_Rep /= "-" then

                     Put_Line
                       (Output_File,
                        "pragma Source_File_Name (" &
                        File_To_Unit
                          (Sources (F).Display_Base_Name,
                           Suffix, Dot_Rep) &
                        ",");
                     Put_Line
                       (Output_File,
                        "Spec_File_Name => """  &
                        Sources (F).Display_Base_Name &
                        """);");

                  end if;
               end;

            end if;

         end if;

         if
           Source_Project_Tree.Info (Sources (F)).Unit_Part = Unit_Body
         then
            Cur := Specific_Sources.Find (Sources (F).Display_Base_Name);

            if Cur = Specific_Sources_Set.No_Element then

               Project :=
                 GNATCOLL.Projects.Project
                   (Source_Project_Tree.Info (Sources (F)));
               declare
                  Suffix  : constant String :=
                    Attribute_Value (Project, Impl_Suffix_Attribute, "ada");
                  Dot_Rep : constant String :=
                    Attribute_Value
                      (Project, Dot_Replacement_Attribute, "ada");
               begin
                  if Suffix /= ".adb" or else Dot_Rep /= "-" then

                     Put_Line
                       (Output_File,
                        "pragma Source_File_Name (" &
                        File_To_Unit
                          (Sources (F).Display_Base_Name,
                           Suffix, Dot_Rep) &
                        ",");
                     Put_Line
                       (Output_File,
                        "Body_File_Name => """  &
                        Sources (F).Display_Base_Name &
                        """);");

                  end if;
               end;

            end if;

         end if;

      end loop;

      Close (Output_File);

      Specific_Sources.Clear;

   end Get_Naming_Info;

   -------------------------
   -- Is_Externally_Built --
   -------------------------
   function Is_Externally_Built (File : Virtual_File) return Boolean is
      F_Info : constant File_Info    := Info (Source_Project_Tree, File);
      Proj   : constant Project_Type := Project (F_Info);
      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      if Has_Attribute (Proj, Attr) then
         if To_Lower (Attribute_Value (Proj, Attr)) = "true" then
            return True;
         end if;
      end if;
      return False;
   end Is_Externally_Built;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Parse_Config_File;
      Scan_Parameters;
      case GNATtest_Mode is
         when Generation =>
            Check_Parameters;
         when Aggregation =>
            Check_Aggregation_Parameters;
      end case;
   exception
      when Parameter_Error =>
         --  The diagnosis is already generated
         raise Fatal_Error;
      when others =>
         Report_Err ("gnattest: initialization failed");
         --  Exception info will be generated in main driver
         raise;
   end Initialize;

   ---------------------------
   -- Non_Null_Intersection --
   ---------------------------

   function Non_Null_Intersection
     (Left  : File_Array_Access;
      Right : File_Array_Access) return Boolean is
   begin
      for J in Left'Range loop
         declare
            Left_Str : constant String :=
                         Normalize_Pathname
                           (Name => Left.all (J).Display_Full_Name,
                            Case_Sensitive => False);
         begin
            for K in Right'Range loop
               if Left_Str =
                 Normalize_Pathname
                   (Name => Right.all (K).Display_Full_Name,
                    Case_Sensitive => False)
               then
                  return True;
               end if;
            end loop;
         end;
      end loop;

      return False;
   end Non_Null_Intersection;

   -----------------------------
   -- Process_Exclusion_Lists --
   -----------------------------

   procedure Process_Exclusion_Lists is
      F : File_Type;
      function Is_Comment (S : String) return Boolean is
        (S'Length >= 2 and then S (S'First .. S'First + 1) = "--");
      S : String_Access;

      use String_To_String_Map;
      Cur : String_To_String_Map.Cursor := Stub_Exclusion_List_Files.First;
   begin
      Change_Dir (Run_Dir.all);
      if Default_Stub_Exclusion_List_File /= null then
         declare
            F_Path : constant String :=
              Normalize_Pathname
                (Name           => Default_Stub_Exclusion_List_File.all,
                 Case_Sensitive => False);
         begin
            if not Is_Regular_File (F_Path) then
               Report_Err ("gnattest: cannot find " & F_Path);
               raise Parameter_Error;
            end if;
            Open (F, In_File, F_Path);
            while not End_Of_File (F) loop
               S := new String'(Get_Line (F));
               if not Is_Comment (S.all) then
                  Store_Default_Excluded_Stub (S.all);
               end if;
               Free (S);
            end loop;
            Close (F);
         end;
      end if;

      while Cur /= String_To_String_Map.No_Element loop
         declare
            Unit : constant String := Key (Cur);
            F_Path : constant String :=
              Normalize_Pathname
                (Name           => Element (Cur),
                 Case_Sensitive => False);
         begin
            if not Is_Regular_File (F_Path) then
               Report_Err ("gnattest: cannot find " & F_Path);
               raise Parameter_Error;
            end if;
            Open (F, In_File, F_Path);
            while not End_Of_File (F) loop
               S := new String'(Get_Line (F));
               if not Is_Comment (S.all) then
                  Store_Excluded_Stub (Unit, S.all);
               end if;
               Free (S);
            end loop;
            Close (F);
         end;

         Next (Cur);
      end loop;

      Change_Dir (Temp_Dir.all);
   end Process_Exclusion_Lists;

   -------------------------------------
   -- Read_Sources_From_Binder_Output --
   -------------------------------------

   procedure Read_Sources_From_Binder_Output is
      Command     :          String_Access;
      Target      : constant String := ASIS_UL.Common.Detect_Target;
      Return_Code :          Integer;

      Bind_Out_File_Name : constant String := Temp_Dir.all & ".bind_out";
      Success        : Boolean;

      Cur_Idx : Natural := 1;
      Last_Idx : constant Natural :=
        11 + Integer (Ext_Var_Buffer.Length) + (if Target = "" then 0 else 1) +
        (if RTS_Path.all = "" then 0 else 1);

      Args : Argument_List (1 .. Last_Idx);

      Cur : List_Of_Strings.Cursor := Ext_Var_Buffer.First;

      Program_Output_File : File_Type;
      Str : String_Access;
      VF : Virtual_File;
   begin
      Command := Locate_Exec_On_Path ("gprbuild");

      Args (Cur_Idx) := new String'("-f");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-q");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-b");
      Cur_Idx := Cur_Idx + 1;
      if Target /= "" then
         Args (Cur_Idx) :=
           new String'("--target=" & Target (Target'First .. Target'Last - 1));
         Cur_Idx := Cur_Idx + 1;
      end if;
      Args (Cur_Idx) := new String'("-P");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'(Source_Prj.all);
      Cur_Idx := Cur_Idx + 1;
      while Cur /= List_Of_Strings.No_Element loop
         Args (Cur_Idx) := new String'("-X" & List_Of_Strings.Element (Cur));
         Cur_Idx := Cur_Idx + 1;
         Next (Cur);
      end loop;
      if RTS_Path.all /= "" then
         Args (Cur_Idx) :=
           new String'("--RTS=" & RTS_Path.all);
         Cur_Idx := Cur_Idx + 1;
      end if;
      Args (Cur_Idx) := new String'("-gnatws");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'
        (Source_Project_Tree.Create (+Main_Unit.all).Display_Full_Name);
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-bargs");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-ws");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-R");
      Cur_Idx := Cur_Idx + 1;
      Args (Cur_Idx) := new String'("-Z");

      Trace (Me, "Switches for closure:");
      Increase_Indent (Me);
      for I in Args'Range loop
         Trace (Me, Args (I).all);
      end loop;
      Decrease_Indent (Me);

      Spawn
        (Program_Name => Command.all,
         Args         => Args,
         Output_File  => Bind_Out_File_Name,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => False);

      if Success and then Return_Code = 0 then

         Open (Program_Output_File, In_File, Bind_Out_File_Name);

         while not End_Of_File (Program_Output_File) loop
            Str := new String'(Get_Line (Program_Output_File));
            VF := Source_Project_Tree.Create (+Str.all);

            if Source_Project_Tree.Info (VF).Unit_Part = Unit_Spec then
               GNATtest.Skeleton.Source_Table.Add_Source_To_Process
                 (VF.Display_Full_Name);
            end if;

            Free (Str);
         end loop;

         Close (Program_Output_File);
      else
         Error_No_Tool_Name ("could not get closure of " & Main_Unit.all);
         raise Fatal_Error;
      end if;

   end Read_Sources_From_Binder_Output;

   -------------------------------------------
   -- Register_Gnattest_Specific_Attributes --
   -------------------------------------------

   procedure Register_Gnattest_Specific_Attributes is
      Dummy : String_Access;
   begin
      Dummy := new String'
        (Register_New_Attribute
           (Name => "harness_dir",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "subdir",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "tests_root",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "tests_dir",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "additional_tests",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "skeletons_default",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "gnattest_switches",
            Pkg  => "gnattest",
            Is_List => True));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name => "default_stub_exclusion_list",
            Pkg  => "gnattest"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      Dummy := new String'
        (Register_New_Attribute
           (Name    => "stub_exclusion_list",
            Pkg     => "gnattest",
            Indexed => True));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

      --  Not really a gnattest specific attribute, but we still need to
      --  inherit makefile attribute in test driver.
      Dummy := new String'
        (Register_New_Attribute
           (Name => "makefile",
            Pkg  => "make"));
      if Dummy.all /= "" then
         Report_Err ("gnattest: cannot parse project file");
         Report_Err (Dummy.all);
         raise Fatal_Error;
      end if;
      Free (Dummy);

   end Register_Gnattest_Specific_Attributes;

   --------------------
   -- Scan_Dir_Affix --
   --------------------

   procedure Scan_Dir_Affix (Affix : String) is
      First_Idx : constant Integer := Affix'First;
      Last_Idx  : constant Integer := Affix'Last;
   begin
      for Idx in First_Idx .. Last_Idx loop
         if Affix (Idx) = '*' then
            Free (Test_Dir_Prefix);
            Test_Dir_Prefix := new String'(Affix (First_Idx .. Idx - 1));
            Free (Test_Dir_Suffix);
            Test_Dir_Suffix := new String'(Affix (Idx + 1 .. Last_Idx));
            return;
         end if;
      end loop;

      Report_Err ("gnattest: invalid parallel dir affix, should contain *");
      raise Parameter_Error;
   end Scan_Dir_Affix;

   ---------------------
   -- Scan_Parameters --
   ---------------------

   procedure Scan_Parameters is
      Multiple_Output : Boolean := False;
      Main_Unit_Set   : Boolean := False;

      Possible_Switches : constant String :=
        "h d? P: q -tests-root= "
        & "-help -version "
        & "-separates "
        & "-no-separates "
        & "-transition "
        & "-subdir= r v X? U: "
        & "-harness-only "
        & "-stub "
        & "-separate-drivers? "
        & "-skeleton-default= "
        & "-passed-tests= "
        & "-exit-status= "
        & "-omit-sloc "
        & "-test-duration "
        & "-validate-type-extensions "
        & "-additional-tests= "
        & "-harness-dir= "
        & "-tests-dir= "
        & "-stubs-dir= "
        & "-queues= j? "
        & "-RTS= "
        & "-no-command-line "
        & "-exclude-from-stubbing?";

      Ignore_Arg : String_Access := new String'("");

      function Get_GNATtest_Mode return GNATtest_Modes;

      procedure Report_Switch (S : String; Expected_Mode : GNATtest_Modes);

      procedure Process_Stub_Exclusion (S : String);

      function Get_GNATtest_Mode return GNATtest_Modes is
         Anything    : Boolean := False;
         Opt_Counter : Natural := 0;
         V_Set       : Boolean := False;
      begin
         Initialize_Option_Scan
           (Stop_At_First_Non_Switch => False,
            Section_Delimiters       => "cargs");

         loop
            case GNAT.Command_Line.Getopt (Possible_Switches) is
               when ASCII.NUL =>
                  exit;
               when 'P' =>
                  return Generation;
               when 'v' =>
                  Anything := True;
                  V_Set := True;
               when others =>
                  Opt_Counter := Opt_Counter + 1;
                  Anything := True;
            end case;
         end loop;

         Free (Ignore_Arg);
         Ignore_Arg := new String'(Get_Argument);

         if not Anything and then Ignore_Arg.all = "" then
            Brief_Help;
            raise Parameter_Error;
         end if;

         if File_Extension (Ignore_Arg.all) = ".gpr" then
            Source_Prj := new String'(Ignore_Arg.all);
            return Generation;
         end if;

         if File_Extension (Ignore_Arg.all) in ".adb" | ".ads" then
            Report_Err
              ("gnattest: project file not specified");
            raise Parameter_Error;
         end if;

         if V_Set and then Ignore_Arg.all = "" and then Opt_Counter = 0 then
            Print_Version_Info (2011);
            OS_Exit (0);
         end if;
         return Aggregation;
      end Get_GNATtest_Mode;

      procedure Process_Stub_Exclusion (S : String) is
         Idx : Natural;
         Valid_Format : constant Boolean :=
           S'Length > 1 and then
           ((S (S'First) = '=') or else
              (S (S'First) = ':' and then Index (S, "=") > S'First + 1));
      begin
         if not Valid_Format then
            Report_Err
              ("wrong parameter for --exclude-from-stubbing: " & S);
            raise Parameter_Error;
         end if;
         if S (S'First) = '=' then
            Default_Stub_Exclusion_List_File := new String'
              (S (S'First + 1 .. S'Last));
         elsif S (S'First) = ':' then
            Idx := Index (S, "=");
            Stub_Exclusion_List_Files.Include
              (S (S'First + 1 .. Idx - 1),  S (Idx + 1 .. S'Last));
         else
            Report_Err
              ("wrong parameter for --exclude-from-stubbing: " & S);
            raise Parameter_Error;
         end if;
      end Process_Stub_Exclusion;

      procedure Report_Switch (S : String; Expected_Mode : GNATtest_Modes) is
      begin
         if Expected_Mode /= GNATtest_Mode then
            case GNATtest_Mode is
               when Generation =>
                  Report_Std
                    ("warning: (gnattest) -"
                     & S
                     & " switch is not relevant in generation mode");
               when Aggregation =>
                  Report_Std
                    ("warning: (gnattest) -"
                     & S
                     & " switch is not relevant in aggregation mode");
            end case;
         end if;
      end Report_Switch;
   begin
      GNATtest_Mode := Get_GNATtest_Mode;

      Initialize_Option_Scan
        (Stop_At_First_Non_Switch => False,
         Section_Delimiters       => "cargs");

      loop
         case GNAT.Command_Line.Getopt (Possible_Switches) is
            when ASCII.NUL =>
               exit;

            when 'U' =>
               Main_Unit := new String'(Parameter);
               Main_Unit_Set := True;
               Report_Switch (Full_Switch, Generation);

            when 'd' =>
               if Full_Switch = "d" then
                  Set_Debug_Options (Parameter);
               end if;

            when 'j' =>
               declare
                  N : Natural;
               begin
                  N := Positive'Value (Parameter);
                  if N = 0 then
                     Queues_Number :=
                       Positive (System.Multiprocessors.Number_Of_CPUs);
                  else
                     Queues_Number := N;
                  end if;
               exception
                  when Constraint_Error =>
                     Report_Err
                       ("wrong parameter for -j, "
                        & "should be a natural number");
                     raise Parameter_Error;
               end;
               Report_Switch (Full_Switch, Aggregation);

            when 'P' =>
               Source_Prj := new String'(Parameter);

            when 'q' =>
               Quiet := True;

            when 'v' =>
               Verbose := True;

               Print_Version_Info (2011);

            when 'r' =>
               Recursive_Sources := True;
               Report_Switch (Full_Switch, Generation);

            when '-' =>
               if Full_Switch = "-additional-tests" then
                  Additional_Tests_Prj := new String'(Parameter);
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-passed-tests" then
                  if Parameter = "hide" then
                     Show_Passed_Tests := False;
                     Show_Passed_Tests_Set := True;
                  elsif Parameter = "show" then
                     Show_Passed_Tests := True;
                     Show_Passed_Tests_Set := True;
                  else
                     Report_Err
                       ("gnattest: --passed-tests " &
                        "should be either show or hide");
                     raise Parameter_Error;
                  end if;
               end if;

               if Full_Switch = "-skeleton-default" then
                  if Parameter = "pass" then
                     Skeletons_Fail := False;
                     Default_Skeletons_Set := True;
                  elsif Parameter = "fail" then
                     Skeletons_Fail := True;
                     Default_Skeletons_Set := True;
                  else
                     Report_Err
                       ("gnattest: --skeleton-default " &
                        "should be either fail or pass");
                     raise Parameter_Error;
                  end if;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-exit-status" then
                  if Parameter = "on" then
                     Add_Exit_Status := True;
                     Add_Exit_Status_Set := True;
                  elsif Parameter = "off" then
                     Add_Exit_Status := True;
                     Add_Exit_Status_Set := True;
                  else
                     Report_Err
                       ("gnattest: --exit-code " &
                        "should be either on or off");
                     raise Parameter_Error;
                  end if;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-tests-root" then
                  if not Multiple_Output then
                     Output_M := Separate_Root;
                     Output_M_Set := True;
                     Separate_Root_Dir := new String'(Parameter);
                     Multiple_Output := True;
                  else
                     Report_Err
                       ("gnattest: multiple output modes are not allowed");
                     raise Parameter_Error;
                  end if;
                  Report_Switch (Full_Switch, Generation);
                  Tests_Dir_Set := True;
               end if;

               --  Hidden from usage due to overengineering.
               if Full_Switch = "-parallel" then
                  if not Multiple_Output then
                     Output_M := Parallel;
                     Output_M_Set := True;
                     Scan_Dir_Affix (Parameter);
                     Multiple_Output := True;
                  else
                     Report_Err
                       ("gnattest: multiple output modes are not allowed");
                     raise Parameter_Error;
                  end if;
               end if;

               if Full_Switch = "-subdir" then

                  if not Multiple_Output then
                     Output_M := Subdir;
                     Output_M_Set := True;
                     Test_Subdir_Name := new String'(Parameter);
                     Multiple_Output := True;
                  else
                     Report_Err
                       ("gnattest: multiple output modes are not allowed");
                     raise Parameter_Error;
                  end if;
                  Report_Switch (Full_Switch, Generation);
                  Tests_Dir_Set := True;
               end if;

               if Full_Switch = "-tests-dir" then

                  if not Multiple_Output then
                     Output_M := Direct;
                     Output_M_Set := True;
                     Test_Dir_Name := new String'(Parameter);
                     Multiple_Output := True;
                  else
                     Report_Err
                       ("gnattest: multiple output modes are not allowed");
                     raise Parameter_Error;
                  end if;
                  Report_Switch (Full_Switch, Generation);
                  Tests_Dir_Set := True;
               end if;

               if Full_Switch = "-stubs-dir" then
                  Stub_Dir_Name := new String'(Parameter);
                  Report_Switch (Full_Switch, Generation);
                  Stub_Dir_Set := True;
               end if;

               if Full_Switch = "-validate-type-extensions" then
                  Substitution_Suite := True;
                  Report_Switch (Full_Switch, Generation);
                  if Stub_Mode_ON then
                     Report_Std
                       ("warning: --validate-type-extensions has no effect "
                        & "in --stub mode");
                  end if;
               end if;

               if Full_Switch = "-omit-sloc" then
                  Omit_Sloc := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-test-duration" then
                  Show_Test_Duration := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-harness-dir" then
                  Harness_Dir := new String'(Parameter);
                  Harness_Set_By_Switch := True;
                  Report_Switch (Full_Switch, Generation);
                  Harness_Dir_Set := True;
               end if;

               if Full_Switch = "-harness-only" then
                  Harness_Only := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-separates" then
                  Generate_Separates := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-no-separates" then
                  Generate_Separates := False;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-transition" then
                  Transition := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-stub" then
                  Stub_Mode_ON := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-separate-drivers" then
                  Separate_Drivers_Set_By_Switch := True;
                  declare
                     S : constant String := Parameter;
                  begin
                     Separate_Drivers := True;
                     if S = "" or else S = "=unit" then
                        Driver_Per_Unit := True;
                     elsif S = "=test" then
                        Driver_Per_Unit := False;
                     else
                        Report_Err
                          ("wrong parameter for --separate-drivers");
                        raise Parameter_Error;
                     end if;
                  end;

                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-version" then
                  Print_Tool_Version (2011);
                  OS_Exit (0);
               end if;

               if Full_Switch = "-help" then
                  Brief_Help;
                  raise Parameter_Error;
               end if;

               if Full_Switch = "-queues" then
                  declare
                     N : constant Natural := Positive'Value (Parameter);
                  begin
                     if N = 0 then
                        Queues_Number :=
                          Positive (System.Multiprocessors.Number_Of_CPUs);
                     else
                        Queues_Number := N;
                     end if;
                  exception
                     when Constraint_Error =>
                        Report_Err
                          ("wrong parameter for --queues, "
                           & "should be a natural number");
                        raise Parameter_Error;
                  end;
                  Report_Switch (Full_Switch, Aggregation);
               end if;

               if Full_Switch = "-RTS" then
                  Free (RTS_Path);
                  RTS_Path := new String'(Parameter);
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-no-command-line" then
                  No_Command_Line_Externally_Set := True;
                  Report_Switch (Full_Switch, Generation);
               end if;

               if Full_Switch = "-exclude-from-stubbing" then
                  Process_Stub_Exclusion (Parameter);
               end if;

            when 'X' =>
               Ext_Var_Buffer.Append (Parameter);

            when others =>
               raise Parameter_Error;
         end case;
      end loop;

      loop
         declare
            Temp : constant String := Get_Argument;
         begin
            if Temp = "" then
               exit;
            end if;
            if Temp /= Ignore_Arg.all or else GNATtest_Mode = Aggregation then
               if Main_Unit_Set then
                  Report_Err
                    ("cannot specify argument sources after -U is used");
                  raise Parameter_Error;
               else
                  Source_Buffer.Append (Temp);
               end if;
            end if;
         end;
      end loop;

      Process_cargs_Section;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Report_Err ("gnattest: invalid switch : " & Full_Switch);
         Report_Err ("Try `gnattest --help` for more information.");

         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Report_Err ("gnattest: missing parameter for: -" & Full_Switch);
         Report_Err ("Try `gnattest --help` for more information.");

         raise Parameter_Error;
   end Scan_Parameters;

   ------------------------------
   -- Has_Command_Line_Support --
   ------------------------------

   function Has_Command_Line_Support return Boolean is
      Files : constant GNATCOLL.VFS.File_Array :=
        Predefined_Source_Files (Env);
   begin
      if No_Command_Line_Externally_Set then
         return False;
      end if;
      for I in Files'Range loop
         if Files (I).Display_Base_Name = "a-comlin.ads" then
            return True;
         end if;
      end loop;
      return False;
   end Has_Command_Line_Support;

end GNATtest.Environment;
