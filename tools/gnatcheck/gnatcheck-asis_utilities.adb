------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--             G N A T C H E C K . A S I S _ U T I L I T I E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Wide_Text_IO;           use Ada.Wide_Text_IO;
--  with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
--  with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Asis.Clauses;               use Asis.Clauses;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;
with Asis.Exceptions;
with Asis.Expressions;           use Asis.Expressions;
with Asis.Extensions;            use Asis.Extensions;
with Asis.Extensions.Flat_Kinds; use Asis.Extensions.Flat_Kinds;
with Asis.Iterator;              use Asis.Iterator;
with Asis.Statements;            use Asis.Statements;
with Asis.Text;                  use Asis.Text;

--  with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
--  with ASIS_UL.Source_Table;       use ASIS_UL.Source_Table;
with ASIS_UL.Utilities;          use ASIS_UL.Utilities;

with Table;

with Atree;                      use Atree;
with Einfo;                      use Einfo;
with Namet;                      use Namet;
with Nlists;                     use Nlists;
with Sem_Aux;                    use Sem_Aux;
with Sinfo;                      use Sinfo;
with Snames;                     use Snames;
with Stand;                      use Stand;
with Types;                      use Types;

with Asis.Set_Get;               use Asis.Set_Get;

with A4G.A_Sem;                  use A4G.A_Sem;
with A4G.Vcheck;                 use A4G.Vcheck;

with Gnatcheck.Traversal_Stack;  use Gnatcheck.Traversal_Stack;

package body Gnatcheck.ASIS_Utilities is
   Package_Name : constant String := "Gnatcheck.ASIS_Utilities";

   -------------------------
   -- ASIS Elements Table --
   -------------------------

   --  Here we define the same structure as A4G.Asis_Tables.Asis_Element_Table.
   --  We need it to create the results of the functions returning
   --  Element_List, but we can not reuse A4G.Asis_Tables.Asis_Element_Table
   --  because it may be used by the standard ASIS queries we may need for our
   --  gnatcheck ASIS utilities.

   package Gnatcheck_Element_Table is new Table.Table (
     Table_Component_Type => Asis.Element,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "GNATCHECK Element List");

   -----------------------
   -- Local subprograms --
   -----------------------

   function Is_Limited (SM : Asis.Element) return Boolean;
   --  Supposing that SM represent a subtype mark, checks if the denoted type
   --  is limited. Returns False for any unexpected element.
   --
   --  Expected Expression_Kinds:
   --       An_Identifier
   --       A_Selected_Component
   --       An_Attribute_Reference

   function Is_Constr_Error_Declaration (Decl : Asis.Element) return Boolean;
   function Is_Num_Error_Declaration (Decl : Asis.Element) return Boolean;
   --  Checks if the argument represents the declaration of the predefined
   --  exception Constraint_Error/Numeric_Error

   function Is_Task_Object_Declaration (Expr : Asis.Element) return Boolean;
   --  Check if the element if a declaration of (one or more) task object(s)
   --  Returns False for any unexpected object
   --
   --  Expected Declaration_Kinds:
   --       A_Variable_Declaration
   --       A_Constant_Declaration

   function Get_Called_Task (Call : Asis.Element) return Asis.Element;
   pragma Unreferenced (Get_Called_Task);
   --  Provided that Is_Task_Entry_Call (Call) computes the called
   --  task.
   --  What is "the called task" for different ways of defining a task
   --  object ???

   procedure Look_For_Loop_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Actual for Traverse_Element instantiation.
   --  Terminates the traversal and sets State ON when visiting a loop
   --  statement. Skips traversal of declarations, expressions and simple
   --  statements

   procedure Empty_Bool_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  Actual for Traverse_Element instantiation.
   --  Does nothing.

   procedure Look_For_Loop is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Look_For_Loop_Pre_Op,
      Post_Operation    => Empty_Bool_Post_Op);
   --  Looks for a lood statement enclosed by its Element argument and sets
   --  the result of the search to its State parameter. Declarations are not
   --  searched.

   procedure Check_For_Discr_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean);
   --  If Element is An_Identifier, checks if it is reference to discriminant;
   --  and if it is - sets State ON and terminates traversing

   procedure Check_For_Discriminant_Reference is new Traverse_Element
     (State_Information => Boolean,
      Pre_Operation     => Check_For_Discr_Reference,
      Post_Operation    => Empty_Bool_Post_Op);
   --  Checks if Element has a reference to a discriminant

   ---------------------------
   -- Can_Cause_Side_Effect --
   ---------------------------

   function Can_Cause_Side_Effect (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Boolean := False;
   begin
      --  !!! Only partial implementation for now!!!

      case Arg_Kind is
         when An_Assignment_Statement    |
              A_Procedure_Call_Statement |
              A_Function_Call            =>
            --  What about entry calls???
            Result := True;
--         when =>
         when others =>
            null;
      end case;

      return Result;
   end Can_Cause_Side_Effect;

   ----------------------------------------------
   -- Call_To_Complicated_Cuncurrent_Structure --
   ----------------------------------------------

   function Call_To_Complicated_Cuncurrent_Structure
     (Call : Asis.Element)
      return Boolean
   is
      Arg_Kind    : constant Flat_Element_Kinds := Flat_Element_Kind (Call);
      Result      : Boolean                     := True;
      Called_Pref : Asis.Element                := Nil_Element;
      Called_Obj  : Asis.Element                := Nil_Element;
      Tmp_El      : Asis.Element;
   begin

      case Arg_Kind is
         when An_Entry_Call_Statement    |
             A_Procedure_Call_Statement =>
            Called_Pref := Called_Name (Call);

            if Arg_Kind = An_Entry_Call_Statement
             and then
               Flat_Element_Kind (Called_Pref) = An_Indexed_Component
            then
               --  Call to an entry from an entry family
               Called_Pref := Prefix (Called_Pref);
            end if;

         when A_Function_Call =>
            Called_Pref := Prefix (Call);
         when others =>
            null;
      end case;

      --  Called_Pref should be of A_Selected_Component kind. We are interested
      --  in task or protected object now

      if Flat_Element_Kind (Called_Pref) = A_Selected_Component then
         Called_Pref := Prefix (Called_Pref);

         if Flat_Element_Kind (Called_Pref) = A_Selected_Component then
            Called_Pref := Selector (Called_Pref);
         end if;

      end if;

      if Expression_Kind (Called_Pref) = An_Identifier then

         begin
            Called_Obj := Corresponding_Name_Definition (Called_Pref);
         exception
            when others =>
               Called_Obj := Nil_Element;
         end;

      end if;

      if not Is_Nil (Called_Obj) then
         Tmp_El := Enclosing_Element (Called_Obj);

         case Declaration_Kind (Tmp_El) is
            when A_Single_Task_Declaration .. A_Single_Protected_Declaration =>
               Result := False;

            when A_Variable_Declaration | A_Constant_Declaration =>
               Tmp_El := Object_Declaration_View (Tmp_El);

               Tmp_El := Asis.Definitions.Subtype_Mark (Tmp_El);

               if Expression_Kind (Tmp_El) = A_Selected_Component then
                  Tmp_El := Selector (Tmp_El);
               end if;

               Tmp_El := Corresponding_Name_Declaration (Tmp_El);

               --  Now we check that the type of the object is a task or
               --  protected type

               Tmp_El := Corresponding_First_Subtype (Tmp_El);

               --  We can n0t have a private type here.

               if Declaration_Kind (Tmp_El) in
                 A_Task_Type_Declaration .. A_Protected_Type_Declaration
               then
                  Result := False;
               else
                  Tmp_El := Type_Declaration_View (Tmp_El);

                  if Asis.Elements.Type_Kind (Tmp_El) =
                    A_Derived_Type_Definition
                  then
                     Tmp_El := Corresponding_Root_Type (Tmp_El);

                     if Declaration_Kind (Tmp_El) in
                       A_Task_Type_Declaration .. A_Protected_Type_Declaration
                     then
                        Result := False;
                     end if;

                  end if;
               end if;

            when others =>
               null;
         end case;

      end if;

      return Result;
   end Call_To_Complicated_Cuncurrent_Structure;

   -----------------------------------
   -- Can_Be_Replaced_With_Function --
   -----------------------------------

   function Can_Be_Replaced_With_Function
     (Decl : Asis.Element)
      return Boolean
   is
      Out_Par : Asis.Element := Nil_Element;
      Result  : Boolean := False;
   begin

      case Declaration_Kind (Decl) is
         when A_Procedure_Declaration         |
              A_Procedure_Body_Declaration    |
              A_Procedure_Body_Stub           |
              A_Generic_Procedure_Declaration |
              A_Formal_Procedure_Declaration  =>

            declare
               Params : constant Asis.Element_List := Parameter_Profile (Decl);
            begin

               for J in Params'Range loop

                  case Mode_Kind (Params (J)) is
                     when An_Out_Mode =>

                        if Names (Params (J))'Length > 1 then
                           Result := False;
                           exit;
                        end if;

                        if Is_Nil (Out_Par) then
                           Out_Par := Object_Declaration_View (Params (J));

                           if Definition_Kind (Out_Par) =
                                 An_Access_Definition
                           then
                              Result := True;
                           else
                              --  If we are here, Out_Par represents a subtype
                              --  mark
                              Result := not Is_Limited (Out_Par);

                              exit when not Result;

                           end if;

                        else
                           Result := False;
                           exit;
                        end if;

                     when An_In_Out_Mode =>
                        Result := False;
                        exit;
                     when others =>
                        null;
                  end case;

               end loop;

            end;

         when others =>
            null;
      end case;

      return Result;
   end Can_Be_Replaced_With_Function;

   ---------------------
   -- Changed_Element --
   ---------------------

   function Changed_Element (El : Asis.Element) return Asis.Element is
      Arg_Elem :          Asis.Element       := El;
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Asis.Element       := Nil_Element;
   begin

      --  Problem with access types!!!???

      case Arg_Kind is
         when An_Identifier =>
            --  Nothing to do:
            null;
         when A_Selected_Component =>
            Arg_Elem := Get_Whole_Object (Arg_Elem);

         when An_Indexed_Component    |
              A_Slice                 |
              An_Explicit_Dereference =>

            while not (Expression_Kind (Arg_Elem) = A_Selected_Component
                   or else
                       Expression_Kind (Arg_Elem) = An_Identifier)
            loop
               Arg_Elem := Prefix (Arg_Elem);
            end loop;

            if Expression_Kind (Arg_Elem) = A_Selected_Component then
               Arg_Elem := Get_Whole_Object (Arg_Elem);
            end if;

         when A_Type_Conversion =>
            return Changed_Element (Converted_Or_Qualified_Expression (El));

--         when  =>
         when others =>
            pragma Assert (False);
            null;
      end case;

      if Expression_Kind (Arg_Elem) = An_Identifier then
         Result := Corresponding_Name_Definition (Arg_Elem);
      else
         Result := Arg_Elem;
      end if;

      return Result;
   end Changed_Element;

   -------------------------------
   -- Check_For_Discr_Reference --
   -------------------------------

   procedure Check_For_Discr_Reference
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin

      case Expression_Kind (Element) is
         when An_Identifier =>

            begin
               if Declaration_Kind (Corresponding_Name_Declaration (Element)) =
                    A_Discriminant_Specification
               then
                  State   := True;
                  Control := Terminate_Immediately;
               end if;
            exception
               when Asis.Exceptions.ASIS_Inappropriate_Element =>
                  null;
            end;

         when Not_An_Expression =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;

   end Check_For_Discr_Reference;

   ----------------------------------------
   -- Constraint_Depends_On_Discriminant --
   ----------------------------------------

   function Constraint_Depends_On_Discriminant
     (Constr : Asis.Element)
      return   Boolean
   is
      Control : Traverse_Control := Continue;
      Result  : Boolean          := False;
   begin

      if Constraint_Kind (Constr) in
           An_Index_Constraint .. A_Discriminant_Constraint
        and then
         Definition_Kind (Enclosing_Element (Enclosing_Element (Constr))) =
           A_Component_Definition
      then
         Check_For_Discriminant_Reference
           (Element => Constr,
            Control => Control,
            State   => Result);
      end if;

      return Result;
   end Constraint_Depends_On_Discriminant;

   -------------------
   -- Contains_Loop --
   -------------------

   function Contains_Loop (El : Asis.Element) return Boolean is
      Control : Traverse_Control := Continue;
      Result  : Boolean          := False;

      Comps : constant Asis.Element_List := Components (El);
   begin

      --  We can not just apply Look_For_Loop tp El - if El itself is a loop
      --  statement, then Result will alvays be True:
      for J in Comps'Range loop
         Look_For_Loop (Comps (J), Control, Result);
         exit when Result;
      end loop;

      return Result;

   end Contains_Loop;

   ------------------------------------
   -- Corresponding_Protected_Object --
   ------------------------------------

   function Corresponding_Protected_Object
     (Pref : Asis.Element)
      return Asis.Element
   is
      Tmp    : Asis.Element := Pref;
      Result : Asis.Element := Nil_Element;
   begin

      if Expression_Kind (Tmp) = A_Function_Call then
         Tmp := Prefix (Tmp);
      else
         Tmp := Called_Name (Tmp);
      end if;

      --  At the moment the simplest case only is implemented: we can process
      --  only the argument Element of the form P_Obj_Name.P_Op_Name

      if Expression_Kind (Tmp) = A_Selected_Component then
         Tmp := Prefix (Tmp);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Selector (Tmp);
         end if;

         pragma Assert (Expression_Kind (Tmp) = An_Identifier);

         Result := Corresponding_Name_Definition (Tmp);

         if Declaration_Kind (Enclosing_Element (Result)) =
            A_Single_Protected_Declaration
         then
            Result := Enclosing_Element (Result);
         end if;

      end if;

      pragma Assert (not Is_Nil (Result));

      return Result;

   end Corresponding_Protected_Object;

   -----------------------------------
   -- Declaration_Of_Renamed_Entity --
   -----------------------------------

   function Declaration_Of_Renamed_Entity
     (R    : Asis.Element)
      return Asis.Element
   is
      Arg_Element : Asis.Element := Renamed_Entity (R);
      Result      : Asis.Element := Nil_Element;
   begin

      if Expression_Kind (Arg_Element) = A_Selected_Component then
         Arg_Element := Selector (Arg_Element);
      end if;

      case Expression_Kind (Arg_Element) is
         when An_Identifier          |
              An_Operator_Symbol     |
              A_Character_Literal    |
              An_Enumeration_Literal =>
            Result := Corresponding_Name_Declaration (Arg_Element);
         when others =>
            null;
      end case;

      return Result;
   exception
      when others =>
         return Nil_Element;
   end Declaration_Of_Renamed_Entity;

   ------------------------
   -- Defines_Components --
   ------------------------

   function Defines_Components (Decl : Asis.Element) return Boolean is
      Type_Def : Asis.Element;
      Result   : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Ordinary_Type_Declaration then

         Type_Def := Type_Declaration_View (Decl);

         case Asis.Elements.Type_Kind (Type_Def) is
            when A_Derived_Record_Extension_Definition |
                 A_Record_Type_Definition              |
                 A_Tagged_Record_Type_Definition       =>
               Result := True;
            when others =>
               null;
         end case;

      end if;

      return Result;

   end Defines_Components;

   ----------------------------
   -- Denotes_Access_Subtype --
   ----------------------------

   function Denotes_Access_Subtype (N : Asis.Element) return Boolean is
   begin
      return Ekind (Node (N)) in Access_Kind;
   end Denotes_Access_Subtype;

   --------------------------------
   -- Denotes_Class_Wide_Subtype --
   --------------------------------

   function Denotes_Class_Wide_Subtype (N : Asis.Element) return Boolean is
      E      : Entity_Id;
      Result : Boolean := False;
   begin

      E := R_Node (N);

      if Nkind (E) in  N_Expanded_Name | N_Identifier then
         E := Entity (E);

         if Present (E) then
            Result := Ekind (E) = E_Class_Wide_Subtype;
         end if;
      end if;

      return Result;
   end Denotes_Class_Wide_Subtype;

   ---------------------------
   -- Empty_Bool_Post_Op --
   ---------------------------

   procedure Empty_Bool_Post_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Empty_Bool_Post_Op;

   -----------------------
   -- Full_View_Visible --
   -----------------------

   function Full_View_Visible
     (Type_Decl : Asis.Declaration;
      At_Place  : Asis.Element)
      return      Boolean
   is
      Result              : Boolean := False;
      Full_View           : Asis.Declaration;
      Enclosing_Pack_Spec : Asis.Declaration;
      Enclosing_Pack_Body : Asis.Declaration;

      Type_Spec_CU     : Asis.Compilation_Unit;
      Type_Body_CU     : Asis.Compilation_Unit := Nil_Compilation_Unit;
      Location_CU      : Asis.Compilation_Unit;
      Next_Parent      : Asis.Compilation_Unit;

      Stub_El          : Asis.Element;
   begin
      --  First, check if we have expected elements and return False if we
      --  do not.

      if Declaration_Kind (Type_Decl) not in
           A_Private_Type_Declaration .. A_Private_Extension_Declaration
        or else
         Is_Part_Of_Implicit (Type_Decl)
        or else
         Is_Part_Of_Implicit (At_Place)
        or else
         Is_Part_Of_Instance (Type_Decl)
        or else
         Is_Part_Of_Instance (At_Place)
      then
         return False;
      end if;

      Full_View           := Corresponding_Type_Declaration (Type_Decl);
      Enclosing_Pack_Spec := Enclosing_Element (Type_Decl);
      Enclosing_Pack_Body := Corresponding_Body (Enclosing_Pack_Spec);

      if Declaration_Kind (Enclosing_Pack_Body) = A_Package_Body_Stub then
         Enclosing_Pack_Body := Corresponding_Subunit (Enclosing_Pack_Body);
      end if;

      Type_Spec_CU := Enclosing_Compilation_Unit (Enclosing_Pack_Spec);
      Location_CU  := Enclosing_Compilation_Unit (At_Place);

      if not Is_Nil (Enclosing_Pack_Body) then
         Type_Body_CU := Enclosing_Compilation_Unit (Enclosing_Pack_Body);
      end if;

      --  Type declaration and location to check are in the same CU:

      if Is_Equal (Type_Spec_CU, Location_CU) then
         if In_Private_Part (Enclosing_Pack_Spec, At_Place) then
            Result := Before (Full_View, At_Place);
         elsif Is_Equal (Type_Body_CU, Location_CU) then
            Result :=
              Inclides (Whole => Enclosing_Pack_Body, Part => At_Place);
         end if;

         return Result;
      end if;

      --  If we are here, then type declaration and location to check are
      --  in different compilation units. First, check if location is in
      --  the body of the package that defines the type. (Subunits are a
      --  pain in this case)

      if not Is_Nil (Type_Body_CU) then

         if not Is_Equal (Type_Body_CU, Location_CU) then

            if Unit_Kind (Location_CU) in A_Subunit then
               Stub_El := Unit_Declaration (Location_CU);
               Stub_El := Corresponding_Body_Stub (Stub_El);
            end if;

            while Unit_Kind (Location_CU) in A_Subunit loop
               exit when Is_Equal (Type_Body_CU, Location_CU);

               Stub_El     := Unit_Declaration (Location_CU);
               Stub_El     := Corresponding_Body_Stub (Stub_El);
               Location_CU := Corresponding_Subunit_Parent_Body (Location_CU);

            end loop;

         else
            Stub_El := At_Place;
         end if;

         if Is_Equal (Type_Body_CU, Location_CU) then
            Result := Inclides (Whole => Enclosing_Pack_Body, Part => Stub_El);
            return Result;
         end if;

      end if;

      --  If we are here, the only possibility when the full view is visible
      --  at a given place is:
      --
      --  - Type_Decl is declared in a visible part of a library package
      --
      --  - At_Place is either in the child unit of this package - either in
      --    the body, or in the private part of the public child, or in the
      --    spec of a private child.

      if (Unit_Kind (Type_Spec_CU) = A_Package
         or else
          Unit_Kind (Type_Spec_CU) = A_Generic_Package)
        and then
          Is_Equal (Enclosing_Element (Type_Decl),
                    Unit_Declaration (Type_Spec_CU))
      then

         while Unit_Kind (Location_CU) in A_Subunit loop
            Location_CU := Corresponding_Subunit_Parent_Body (Location_CU);
         end loop;

         Next_Parent := Location_CU;

         while not Is_Nil (Next_Parent) loop
            exit when Is_Equal (Next_Parent, Type_Spec_CU);
            Next_Parent := Corresponding_Parent_Declaration (Next_Parent);
         end loop;

         if not Is_Equal (Next_Parent, Type_Spec_CU) then
            return False;
         elsif Unit_Kind (Location_CU) in A_Library_Unit_Body then
            return True;
         elsif Unit_Kind (Location_CU) = A_Package
             or else
                Unit_Kind (Location_CU) = A_Generic_Package
         then
            if Unit_Class (Location_CU) = A_Private_Declaration
              and then
               Is_Equal (Corresponding_Parent_Declaration (Location_CU),
                         Type_Spec_CU)
            then
               return True;
            else
               Result :=
                 In_Private_Part (Pack    => Unit_Declaration (Location_CU),
                                  Element => At_Place);
               return Result;
            end if;
         end if;

         pragma Assert (False);
         return False;
      end if;

      return False;
   end Full_View_Visible;

   ----------------------
   -- Get_Associations --
   ----------------------

   function Get_Associations (El : Asis.Element) return Asis.Element_List is
   begin

      case Flat_Element_Kind (El) is
         when A_Record_Aggregate     |
              An_Extension_Aggregate =>
            return Record_Component_Associations (El);
         when A_Positional_Array_Aggregate |
              A_Named_Array_Aggregate      =>
            return Array_Component_Associations (El);
--         when  =>
--            return  (El);
         when others =>
            return Nil_Element_List;
      end case;

   end Get_Associations;

   ----------------------
   -- Get_Call_Element --
   ----------------------

   function Get_Call_Element return Asis.Element is
      Steps_Up     : Elmt_Idx := 0;
      Result       : Asis.Element := Get_Enclosing_Element (Steps_Up);
   begin
      loop
         exit when
            Expression_Kind (Result) = A_Function_Call
           or else
            Element_Kind (Result) /= An_Expression;

         Steps_Up := Steps_Up + 1;
         Result   := Get_Enclosing_Element (Steps_Up);
      end loop;

      return Result;
   end Get_Call_Element;

   ---------------------
   -- Get_Called_Task --
   ---------------------

   function Get_Called_Task (Call : Asis.Element) return Asis.Element is
      Result : Asis.Element := Nil_Element;
      Tmp    : Asis.Element;
      Tmp1   : Asis.Element;
   begin
      --  For now - the simplest case. We consider that the prefix has
      --  the form of Task_Name.Entry_Name

      Tmp := Called_Name (Call);

      if Expression_Kind (Tmp) = An_Indexed_Component then
         --  A call to an entry from an entry family
         Tmp := Prefix (Tmp);
      end if;

      if Expression_Kind (Tmp) = A_Selected_Component then
         Tmp := Prefix (Tmp);

         if Expression_Kind (Tmp) = A_Selected_Component then
            Tmp := Asis.Expressions.Selector (Tmp);
         end if;

         Tmp := Corresponding_Name_Definition (Tmp);

         if not Is_Nil (Tmp) then
            --  For a task declared by a single task declaration we return this
            --  single task declaration, otherwise we return a task defining
            --  identifier
            Tmp1 := Enclosing_Element (Tmp);

            if Declaration_Kind (Tmp1) = A_Single_Task_Declaration then
               Tmp := Tmp1;
            end if;

            Result := Tmp;
         end if;

      end if;

      pragma Assert (not Is_Nil (Result));
      --  A null result requires a special processing, so for the development
      --  period we just blow up

      return Result;
   end Get_Called_Task;

   -----------------
   -- Get_Choices --
   -----------------

   function Get_Choices (El : Asis.Element) return Asis.Element_List is
   begin

      case Association_Kind (El) is
         when An_Array_Component_Association =>
            return Array_Component_Choices (El);
         when A_Record_Component_Association =>
            return Record_Component_Choices (El);
         when others =>
            return Nil_Element_List;
      end case;

   end Get_Choices;

   ----------------------------------
   -- Get_Corresponding_Definition --
   ----------------------------------

   function Get_Corresponding_Definition
     (El   : Asis.Element)
      return Asis.Element
   is
      Arg_Kind : constant Expression_Kinds := Expression_Kind (El);
      Result   : Asis.Element;
   begin

      if not (Arg_Kind = An_Identifier
             or else
              Arg_Kind = An_Operator_Symbol
             or else
              Arg_Kind = A_Character_Literal
             or else
              Arg_Kind = An_Enumeration_Literal)
      then
         --  To avoid junk use of this query
         Raise_ASIS_Inappropriate_Element
           (Diagnosis =>
              "Gnatcheck.ASIS_Utilities.Get_Corresponding_Definition",
            Wrong_Kind => Int_Kind (El));
      end if;

      begin
         Result := Corresponding_Name_Definition (El);
      exception
         when Asis.Exceptions.ASIS_Inappropriate_Element =>
            Result := Nil_Element;
      end;

      return Result;
   end Get_Corresponding_Definition;

   ------------------
   -- Get_Handlers --
   ------------------

   function Get_Handlers
     (El              : Asis.Element;
      Include_Pragmas : Boolean := False)
      return            Asis.Element_List
   is
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              An_Entry_Body_Declaration    |
              A_Task_Body_Declaration      =>
            return Body_Exception_Handlers (El, Include_Pragmas);

         when A_Block_Statement =>
            return Block_Exception_Handlers (El, Include_Pragmas);

         when An_Extended_Return_Statement =>
            return Extended_Return_Exception_Handlers (El, Include_Pragmas);

         when An_Accept_Statement =>
            return Accept_Body_Exception_Handlers (El, Include_Pragmas);

         when others =>
            return Nil_Element_List;
      end case;

   end Get_Handlers;

   -------------------------
   -- Get_Name_Definition --
   -------------------------

   function Get_Name_Definition (Ref : Asis.Element) return Asis.Element is
      Result : Asis.Element := Normalize_Reference (Ref);
   begin

      Result := Corresponding_Name_Definition (Result);

      if Declaration_Kind (Enclosing_Element (Result)) in
           A_Renaming_Declaration
      then
         Result := Corresponding_Base_Entity (Enclosing_Element (Result));
         Result := Normalize_Reference (Result);
         Result := Corresponding_Name_Definition (Result);
      end if;

      return Result;
   end Get_Name_Definition;

   -------------------
   -- Get_Root_Type --
   -------------------

   function Get_Root_Type (Decl : Asis.Element) return Asis.Element is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Decl);
      Type_Def :          Asis.Element;
      Result   :          Asis.Element;
   begin

      case Arg_Kind is
         when A_Variable_Declaration |
              A_Constant_Declaration =>
            null;
         when others =>
            Raise_ASIS_Inappropriate_Element
              (Package_Name & "Get_Root_Type",
               Wrong_Kind => Int_Kind (Decl));
      end case;

      Result := Object_Declaration_View (Decl);
      Result := Asis.Definitions.Subtype_Mark (Result);

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      Result := Corresponding_Name_Declaration (Result);

      if Declaration_Kind (Result) = A_Subtype_Declaration then
         Result := Corresponding_First_Subtype (Result);
      end if;

      if Declaration_Kind (Result) = An_Ordinary_Type_Declaration then
         Type_Def := Type_Declaration_View (Result);

         if Asis.Elements.Type_Kind (Type_Def) in
            A_Derived_Type_Definition .. A_Derived_Record_Extension_Definition
         then
            Result := Corresponding_Root_Type (Type_Def);
         end if;

      end if;

      return Result;

   end Get_Root_Type;

   -------------------------
   -- Get_Type_Components --
   -------------------------

   function Get_Type_Components
     (El                    : Asis.Element;
      Include_Discriminants : Boolean)
      return                  Asis.Element_List
   is
      Type_Def : Asis.Element;

      procedure Add_Components (Comps : Asis.Element_List);
      --  Adds record components to the result, recursively going down into
      --  variant part(s)

      procedure Add_Components (Comps : Asis.Element_List) is
      begin

         for J in Comps'Range loop

            if Declaration_Kind (Comps (J)) = A_Component_Declaration then
               Gnatcheck_Element_Table.Append (Comps (J));
            elsif Definition_Kind (Comps (J)) = A_Variant_Part then

               declare
                  Vars : constant Asis.Element_List := Variants (Comps (J));
               begin
                  for K in Vars'Range loop
                     Add_Components (Record_Components (Vars (K)));
                  end loop;
               end;

            end if;

         end loop;

      end Add_Components;

   begin
      Gnatcheck_Element_Table.Init;

      if Include_Discriminants then

         Type_Def :=  Discriminant_Part (El);

         if Definition_Kind (Type_Def) = A_Known_Discriminant_Part then

            declare
               Discr_List : constant Asis.Element_List :=
                  Discriminants (Type_Def);
            begin

               for J in Discr_List'Range loop
                  Gnatcheck_Element_Table.Append (Discr_List (J));
               end loop;

            end;

         end if;

      end if;

      Type_Def := Type_Declaration_View (El);

      case Flat_Element_Kind (Type_Def) is
         when A_Protected_Definition =>

            declare
               Items : constant Asis.Element_List :=
                 Private_Part_Items (Type_Def);
            begin

               for J in Items'Range loop

                  if Declaration_Kind (Items (J)) =
                     A_Component_Declaration
                  then
                     Gnatcheck_Element_Table.Append (Items (J));
                  end if;

               end loop;

            end;

         when A_Derived_Type_Definition ..
              A_Derived_Record_Extension_Definition =>

            declare
               Items : constant Asis.Element_List :=
                 Implicit_Inherited_Declarations (Type_Def);
            begin

               for J in Items'Range loop

                  if Declaration_Kind (Items (J)) =
                     A_Component_Declaration
                  then
                     Gnatcheck_Element_Table.Append (Items (J));
                  end if;

               end loop;

            end;

         when others =>
            null;
      end case;

      --  Now add explicit record components, if any

      if Asis.Elements.Type_Kind (Type_Def) =
         A_Derived_Record_Extension_Definition
        or else
         Asis.Elements.Type_Kind (Type_Def) = A_Record_Type_Definition
        or else
         Asis.Elements.Type_Kind (Type_Def) = A_Tagged_Record_Type_Definition
      then
         Type_Def := Asis.Definitions.Record_Definition (Type_Def);

         if Definition_Kind (Type_Def) /= A_Null_Record_Definition then

            declare
               Comps : constant Asis.Element_List :=
                 Record_Components (Type_Def);
            begin
               Add_Components (Comps);
            end;

         end if;

      end if;

      return Asis.Element_List
        (Gnatcheck_Element_Table.Table (1 .. Gnatcheck_Element_Table.Last));
   end Get_Type_Components;

   -------------------------------------
   -- Get_Type_Decl_From_Subtype_Mark --
   -------------------------------------

   function Get_Type_Decl_From_Subtype_Mark
     (SM   : Asis.Element)
      return Asis.Element
   is
      Result : Asis.Element := SM;
   begin

      if Expression_Kind (Result) = A_Selected_Component then
         Result := Selector (Result);
      end if;

      Result := Corresponding_Name_Declaration (Result);

      if Declaration_Kind (Result) = A_Subtype_Declaration then
         Result := Corresponding_First_Subtype (Result);
      end if;

      if Declaration_Kind (Result) in
           A_Private_Type_Declaration .. A_Private_Extension_Declaration
      then
         Result := Corresponding_Type_Declaration (Result);
      end if;

      return Result;
   end Get_Type_Decl_From_Subtype_Mark;

   ----------------------
   -- Get_Whole_Object --
   ----------------------

   function Get_Whole_Object (El : Asis.Element) return Asis.Element is
      Pref   : Asis.Element := El;
      --  Pref represents the (left) part of the argument name that has not
      --  been traversed yet

      Result : Asis.Element := Selector (El);
      --  The selector part of the current Pref

      procedure Step_To_The_Left;
      --  Resets the values of Pref and Result, moving them to the beginning
      --  (that is - to the left end) of the name represented by El: as a
      --  result of calling this procedure we should always have Result to be
      --  Selector (Prefix) except we are in the very beginning of El

      procedure Step_To_The_Left is
      begin
         case Expression_Kind (Pref) is
            when Not_An_Expression =>
               --  That is, Pref just is Nil_Element, and we have traversed the
               --  whole name represented by El

               Result := Nil_Element;

            when An_Identifier =>
               --  Approaching the left part of El
               Result := Pref;
               Pref   := Nil_Element;
            when A_Selected_Component =>
               Pref   := Prefix (Pref);

               if Expression_Kind (Pref) = An_Identifier then
                  Result := Pref;
                  Pref := Nil_Element;
               elsif Expression_Kind (Pref) = A_Selected_Component then
                  Result := Selector (Pref);
               else
                  pragma Warnings (Off);
                  Step_To_The_Left;
                  pragma Warnings (On);
               end if;

            when A_Slice                 |
                 An_Explicit_Dereference |
                 An_Indexed_Component    =>
               Pref := Prefix (Pref);

               pragma Warnings (Off);
               Step_To_The_Left;
               pragma Warnings (ON);

            when A_Function_Call =>
               --  A rather exotic case - a function call (or a component
               --  therteof) as a changen element...
               Result := Corresponding_Called_Function (Pref);

            when A_Type_Conversion =>

               Pref := Converted_Or_Qualified_Expression (Pref);

               pragma Warnings (Off);
               Step_To_The_Left;
               pragma Warnings (ON);

            when others =>
               Put_Line (Standard_Error, Debug_Image (Pref));

               if Is_Text_Available (Pref) then
                  Put_Line (Standard_Error, Element_Image (Pref));
               end if;

               pragma Assert (False);
         end case;

      end Step_To_The_Left;

   begin

      while not Is_Nil (Result) loop

         if Is_Function_Declaration (Result) then
            --  Actually, a more detailed analyzis is possible for this case
            exit;
         elsif No (Entity (R_Node (Result)))
           and then
            not Is_Nil (Pref)
         then
            --  We have a case of an expaded name - the Entity field is not
            --  set for a selector, but it is set for a whole expanded name.
            --  So what we now have in Result is what we are looking for:
            exit;

         elsif Is_Nil (Pref) then
            --  That means that we get to the beginning (rightmost identifier)
            --  in the expanded name. It can not be a subcomponent, so:
            exit;
         end if;

         Step_To_The_Left;

      end loop;

      return Result;
   end Get_Whole_Object;

   ------------------------
   -- Has_Address_Clause --
   ------------------------

   function Has_Address_Clause (Def_Name : Asis.Element) return Boolean is
      Object_Decl : constant Asis.Element := Enclosing_Element (Def_Name);

      Corr_Rep_Clauses : constant Asis.Element_List :=
        Corresponding_Representation_Clauses (Object_Decl);

      Result : Boolean := False;
   begin

      for J in Corr_Rep_Clauses'Range loop

         if Representation_Clause_Kind (Corr_Rep_Clauses (J)) =
            An_Attribute_Definition_Clause
           and then
             Attribute_Kind
               (Representation_Clause_Name (Corr_Rep_Clauses (J))) =
            An_Address_Attribute
           and then
             Is_Equal
               (Corresponding_Name_Definition
                 (Prefix (Representation_Clause_Name
                   (Corr_Rep_Clauses (J)))),
                Def_Name)
         then
            Result := True;
            exit;
         end if;

      end loop;

      return Result;
   end Has_Address_Clause;

   -----------------------
   -- Has_One_Parameter --
   -----------------------

   function Has_One_Parameter (El : Asis.Element) return Boolean is
      Template_El : Asis.Element;
      Call_Node   : Node_Id;
      Result      : Boolean := False;
   begin

      if Expression_Kind (El) = A_Function_Call
        or else
         Statement_Kind (El) = A_Procedure_Call_Statement
        or else
         Statement_Kind (El) = An_Entry_Call_Statement
      then
         Call_Node := Node (El);

         if Nkind (Call_Node) = N_Attribute_Reference then

            if Sinfo.Expressions (Call_Node) /= No_List
              and then
               List_Length (Sinfo.Expressions (Call_Node)) = 1
            then
               Result := True;
            end if;

         else

            if Parameter_Associations (Call_Node) /= No_List
              and then
               List_Length (Parameter_Associations (Call_Node)) = 1
            then
               Result := True;
            end if;

         end if;

      elsif Declaration_Kind (El) in A_Generic_Instantiation then
         Template_El := Normalize_Reference (Generic_Unit_Name (El));
         Template_El := Corresponding_Name_Declaration (Template_El);

         if Declaration_Kind (Template_El) in
              A_Generic_Package_Renaming_Declaration ..
              A_Generic_Function_Renaming_Declaration
         then
            Template_El := Corresponding_Base_Entity (Template_El);
            Template_El := Normalize_Reference (Template_El);
            Template_El := Corresponding_Name_Declaration (Template_El);
         end if;

         Result := Generic_Formal_Part (Template_El)'Length = 1;
      end if;

      return Result;
   end Has_One_Parameter;

   --------------------------------
   -- Has_Positional_Association --
   --------------------------------

   function Has_Positional_Association (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Expression_Kind (El) in
           A_Record_Aggregate .. An_Extension_Aggregate
         --  The condition can be extended
      then

         declare
            Associations : constant Asis.Element_List := Get_Associations (El);
         begin
            if Associations'Length > 0 then
               Result := Is_Positional (Associations (Associations'First));
            end if;
         end;

      end if;

      return Result;
   end Has_Positional_Association;

   ------------------------------
   -- Has_Statements_And_Decls --
   ------------------------------

   function Has_Statements_And_Decls (Decl : Asis.Element) return Boolean is
      Result    : Boolean := False;
   begin

      Result := not Is_Nil (Body_Statements (Decl))
              and then
                not Is_Nil (Body_Declarative_Items (Decl));

      return Result;
   end Has_Statements_And_Decls;

   -------------
   -- Is_Body --
   -------------

   function Is_Body (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              A_Task_Body_Declaration      |
              An_Entry_Body_Declaration    =>
            Result := True;
         when  others =>
            null;
      end case;

      return Result;

   end Is_Body;

   ---------------------------
   -- Is_Boolean_Logical_Op --
   ---------------------------

   function Is_Boolean_Logical_Op (Op : Asis.Element) return Boolean is
      Entity_N : Entity_Id;
      Call     : Asis.Element;
      Arg_Node : Node_Id := Node (Op);
      Result   : Boolean := False;
   begin

      if Operator_Kind (Op) in An_And_Operator .. An_Xor_Operator then

         Call := Enclosing_Element (Op);

         if Is_Prefix_Call (Call) then
            Arg_Node := R_Node (Call);

         end if;

         if Nkind (Arg_Node) = N_Op_And
           or else
            Nkind (Arg_Node) = N_Op_Or
           or else
            Nkind (Arg_Node) = N_Op_Xor
         then
            Entity_N := Entity (Arg_Node);

            if Present (Entity_N)
              and then
               Sloc (Entity_N) <= Standard_Location
              and then
               Ekind (Etype (Arg_Node)) = E_Enumeration_Type
            then
               Result := True;
            end if;
         end if;

      end if;

      return Result;
   end Is_Boolean_Logical_Op;

   ----------------------------------
   -- Is_Call_To_Operator_Function --
   ----------------------------------

   function Is_Call_To_Operator_Function (El : Asis.Element) return Boolean is
      Pref   : Asis.Element;
      Result : Boolean := False;
   begin

      if Expression_Kind (El) = A_Function_Call then

         if not Is_Prefix_Call (El) then
            Result := True;
         else
            Pref := Prefix (El);

            if Expression_Kind (Pref) = A_Selected_Component then
               Pref := Selector (Pref);
            end if;

            Result := Expression_Kind (Pref) = An_Operator_Symbol;

         end if;

      end if;

      return Result;
   end Is_Call_To_Operator_Function;

   ---------------
   -- Is_Caller --
   ---------------

--   function Is_Caller (El : Asis.Element) return Boolean is
--      Spec_El : Asis.Element;
--      Result  : Boolean := False;
--   begin
--      --  Implementation is incomplete!!! ???
--      --  Protected operations is a huge hole!!!

--      case Flat_Element_Kind (El) is
--         when A_Procedure_Declaration |
--              A_Function_Declaration  =>

--            Result := Trait_Kind (El) /= An_Abstract_Trait;

--         when An_Entry_Body_Declaration =>

--            Result := True;

--         when A_Procedure_Body_Declaration |
--              A_Function_Body_Declaration  |
--              A_Procedure_Body_Stub        |
--              A_Function_Body_Stub         =>

--            Spec_El := El;

--            if Is_Subunit (El) then
--               Spec_El := Corresponding_Body_Stub (El);
--            end if;

--            Spec_El := Corresponding_Declaration (El);

--            Result :=
--              Declaration_Kind (Spec_El) not in
--                A_Generic_Procedure_Declaration ..
--                A_Generic_Function_Declaration;

--         when An_Entry_Declaration =>

--            if Definition_Kind (Get_Enclosing_Element) =
--               A_Protected_Definition
--            then
--               Result := True;
--            end if;

--         when others =>
--            null;
--      end case;

--      return Result;
--   end Is_Caller;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (E : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      if Defining_Name_Kind (E) = A_Defining_Identifier then
         Result := Ekind (Node (E)) = E_Constant;
      end if;

      return Result;
   end Is_Constant;

   ---------------------------------
   -- Is_Constr_Error_Declaration --
   ---------------------------------

   function Is_Constr_Error_Declaration (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Exception_Declaration
        and then
         Is_Standard (Enclosing_Compilation_Unit (Decl))
        and then
         Defining_Name_Image (First_Name (Decl)) = "Constraint_Error"
      then
         Result := True;
      end if;

      return Result;
   end Is_Constr_Error_Declaration;

   -------------------------
   -- Is_Constraint_Error --
   -------------------------

   function Is_Constraint_Error (Ref : Asis.Element) return Boolean is
      Next_Exception_Decl : Asis.Element;

      Result : Boolean := False;
   begin
      Next_Exception_Decl := Corresponding_Name_Declaration (Ref);

      while not Is_Nil (Next_Exception_Decl) loop

         if Is_Constr_Error_Declaration (Next_Exception_Decl) then
            Result := True;
            exit;
         elsif Is_Num_Error_Declaration (Next_Exception_Decl) then
            exit;
         elsif Declaration_Kind (Next_Exception_Decl) =
               An_Exception_Renaming_Declaration
         then
            Next_Exception_Decl := Renamed_Entity (Next_Exception_Decl);
            Next_Exception_Decl := Normalize_Reference (Next_Exception_Decl);
            Next_Exception_Decl :=
              Corresponding_Name_Declaration (Next_Exception_Decl);
         else
            exit;
         end if;

      end loop;

      return Result;
   end Is_Constraint_Error;

   --------------------------
   -- Is_Control_Structure --
   --------------------------

   function Is_Control_Structure (Stmt : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Statement_Kind (Stmt) is
         when An_If_Statement                    |
              A_Case_Statement                   |
              A_Loop_Statement                   |
              A_While_Loop_Statement             |
              A_For_Loop_Statement               |
              A_Selective_Accept_Statement       |
              A_Timed_Entry_Call_Statement       |
              A_Conditional_Entry_Call_Statement |
              An_Asynchronous_Select_Statement   =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Control_Structure;

   --------------
   -- Is_Frame --
   --------------

   function Is_Frame (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (El) is
         when A_Procedure_Body_Declaration |
              A_Function_Body_Declaration  |
              A_Package_Body_Declaration   |
              An_Entry_Body_Declaration    |
              A_Task_Body_Declaration      |
              A_Block_Statement            |
              An_Extended_Return_Statement |
              An_Accept_Statement          =>

            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Frame;

   ----------------------
   -- Is_From_Standard --
   ----------------------

   function Is_From_Standard (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if not Is_Nil (El) then
         Result := Sloc (Node (El)) <= Standard_Location;
      end if;

      return Result;
   end Is_From_Standard;

   -----------------------------
   -- Is_Function_Declaration --
   -----------------------------

   function Is_Function_Declaration (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Declaration_Kind (El) is
         when A_Function_Declaration          |
              A_Function_Body_Declaration     |
              A_Function_Body_Stub            |
              A_Function_Renaming_Declaration |
              A_Function_Instantiation        |
              A_Formal_Function_Declaration   |
              A_Generic_Function_Declaration  =>

            Result := True;

         when others =>
            null;
      end case;

      return Result;
   end Is_Function_Declaration;

   ---------------------
   -- Is_Dynamic_Call --
   ---------------------

   function Is_Dynamic_Call (Call : Asis.Element) return Boolean is
      Tmp    : Asis.Element;
      Result : Boolean := False;
   begin

      if Expression_Kind (Call) = A_Function_Call then
         Tmp := Prefix (Call);
      else
         Tmp := Called_Name (Call);
      end if;

      if Expression_Kind (Tmp) = An_Explicit_Dereference
        or else
         Is_True_Expression (Tmp)
      then
         --  If the prefix of a (procedure or function) call is a true
         --  expression that is, if it has a type, the only possibility for
         --  this prefix is to be of an access to procedure/function type, so
         Result := True;
      end if;

      return Result;
   end Is_Dynamic_Call;

   ------------------------------
   -- Is_Enum_Literal_Renaming --
   ------------------------------

   function Is_Enum_Literal_Renaming (El : Asis.Element) return Boolean is
      Result         : Boolean := False;
      Renamed_Entity : Entity_Id;
   begin
      if Declaration_Kind (El) = A_Function_Renaming_Declaration then

         Renamed_Entity := Sinfo.Name (Node (El));
         Renamed_Entity := Entity (Renamed_Entity);

         if Present (Renamed_Entity)
           and then
            Ekind (Renamed_Entity) = E_Enumeration_Literal
         then
            Result := True;
         end if;

      end if;

      return Result;
   end Is_Enum_Literal_Renaming;

   --------------
   -- Is_Float --
   --------------

   function Is_Float (Expr : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Type_Entity : Entity_Id;
   begin

      if Asis.Extensions.Is_True_Expression (Expr) then
         Type_Entity := Etype (R_Node (Expr));
         Result      := Ekind (Type_Entity) in Float_Kind;
      end if;

      return Result;

   end Is_Float;

   ----------------
   -- Is_Handled --
   ----------------

   function Is_Handled
     (Exc  : Asis.Element;
      By   : Asis.Element_List)
      return Boolean
   is
      Exc_To_Catch : Asis.Element := Exc;
      Result       : Boolean  := False;
      Last_Handler : Boolean := True;
   begin

      if By'Length > 0 then

         if Declaration_Kind (Enclosing_Element (Exc_To_Catch)) =
            An_Exception_Renaming_Declaration
         then
            Exc_To_Catch :=
              Get_Name_Definition
                (Renamed_Entity (Enclosing_Element (Exc_To_Catch)));
         end if;

         Traverse_Handlers : for J in reverse By'Range loop

            declare
               Handled_Excs : constant Asis.Element_List :=
                 Exception_Choices (By (J));
            begin

               if Last_Handler
                 and then
                  Definition_Kind (Handled_Excs (Handled_Excs'Last)) =
                  An_Others_Choice
               then
                  Result := True;
                  exit Traverse_Handlers;
               end if;

               Last_Handler := False;

               for K in Handled_Excs'Range loop

                  if Is_Equal
                       (Get_Name_Definition (Handled_Excs (K)),
                        Exc_To_Catch)
                  then
                     Result := True;
                     exit Traverse_Handlers;
                  end if;

               end loop;

            end;

         end loop Traverse_Handlers;

      end if;

      return Result;
   end Is_Handled;

   ----------------
   -- Is_Limited --
   ----------------

   function Is_Limited (SM : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      case Expression_Kind (SM) is
         when An_Identifier          |
              A_Selected_Component   |
              An_Attribute_Reference =>

            Type_Entity := Etype (R_Node (SM));

            Result :=
              Is_Limited_Type (Type_Entity)
             or else
              (Is_Interface (Type_Entity)
              and then
               Is_Limited_Interface (Type_Entity));

         when others =>
            null;
      end case;

      return Result;
   end Is_Limited;

   ------------------------------
   -- Is_Num_Error_Declaration --
   ------------------------------

   function Is_Num_Error_Declaration (Decl : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (Decl) = An_Exception_Renaming_Declaration
        and then
         Is_Standard (Enclosing_Compilation_Unit (Decl))
        and then
         Defining_Name_Image (First_Name (Decl)) = "Numeric_Error"
      then
         Result := True;
      end if;

      return Result;
   end Is_Num_Error_Declaration;

   ----------------------
   -- Is_Numeric_Error --
   ----------------------

   function Is_Numeric_Error (Ref : Asis.Element) return Boolean is
      Next_Exception_Decl : Asis.Element;

      Result : Boolean := False;
   begin
      Next_Exception_Decl := Corresponding_Name_Declaration (Ref);

      while not Is_Nil (Next_Exception_Decl) loop

         if Is_Num_Error_Declaration (Next_Exception_Decl) then
            Result := True;
            exit;
         elsif Declaration_Kind (Next_Exception_Decl) =
               An_Exception_Renaming_Declaration
         then
            Next_Exception_Decl := Renamed_Entity (Next_Exception_Decl);
            Next_Exception_Decl := Normalize_Reference (Next_Exception_Decl);
            Next_Exception_Decl :=
              Corresponding_Name_Declaration (Next_Exception_Decl);
         else
            exit;
         end if;

      end loop;

      return Result;
   end Is_Numeric_Error;

   -------------------
   -- Is_Positional --
   -------------------

   function Is_Positional (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if not Is_Normalized (El) then

         case Association_Kind (El) is
            when A_Pragma_Argument_Association |
                 A_Parameter_Association       |
                 A_Generic_Association         =>
               Result := Is_Nil (Formal_Parameter (El));
            when A_Discriminant_Association =>
               Result := Is_Nil (Discriminant_Selector_Names (El));
            when A_Record_Component_Association =>
               Result := Is_Nil (Record_Component_Choices (El));
            when An_Array_Component_Association =>
               Result := Is_Nil (Array_Component_Choices (El));
            when others =>
               null;
         end case;

      end if;

      return Result;
   end Is_Positional;

   -------------------
   -- Is_Predefined --
   -------------------

   function Is_Predefined (Operation : Asis.Element) return Boolean is
      Tmp_Element : Asis.Element;
      Op_Entity   : Entity_Id := Empty;
      Result      : Boolean := False;
   begin

      if Expression_Kind (Operation) = An_Operator_Symbol
        and then
         Is_Uniquely_Defined (Operation)
      then

         Tmp_Element := Corresponding_Name_Definition (Operation);

         if Is_Nil (Tmp_Element) then
            --  This also includes the case of "/=" implicitly declared by
            --  an explicit declaration of "="

            Tmp_Element := Enclosing_Element (Operation);

            if Expression_Kind (Tmp_Element) = A_Selected_Component then
               Op_Entity := R_Node (Tmp_Element);
            else
               Op_Entity := R_Node (Operation);
            end if;

            if Nkind (Op_Entity) = N_Raise_Constraint_Error then
               Op_Entity := Node (Operation);
            end if;

            if Nkind (Op_Entity) = N_Function_Call then
               Op_Entity := Sinfo.Name (Op_Entity);
            end if;

            Op_Entity := Entity (Op_Entity);

            Result := Sloc (Op_Entity) = Standard_Location;

         end if;
      end if;

      return Result;

   end Is_Predefined;

   --------------------------
   -- Is_Predefined_String --
   --------------------------

   function Is_Predefined_String (Type_Decl : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      if Declaration_Kind (Type_Decl) = An_Ordinary_Type_Declaration
        or else
         Declaration_Kind (Type_Decl) = A_Subtype_Declaration
      then
         Type_Entity := R_Node (Names (Type_Decl) (1));

         while Etype (Type_Entity) /= Type_Entity loop
            Type_Entity := Etype (Type_Entity);
         end loop;

         Result := Type_Entity = Stand.Standard_String;

      end if;

      return Result;

   end Is_Predefined_String;

   ----------------------------------
   -- Is_Prefix_Notation_Exception --
   ----------------------------------

   function Is_Prefix_Notation_Exception
     (El                 : Asis.Element;
      Exclude_Second_Par : Boolean)
      return Boolean
   is
      Call_Node      : Node_Id;
      Par_Node       : Node_Id;
      Firts_Par_Node : Node_Id;

      Result    : Boolean := False;
   begin
      Call_Node := Parent (R_Node (El));

      --  We can be sure, that El is a subprogram call that has at least one
      --  parameter, so Parameter_Associations (Call_Node) definitely presents.
      if List_Length (Parameter_Associations (Call_Node)) = 1 then
         Result := True;
      else
         Par_Node       := R_Node (El);
         Firts_Par_Node := First (Parameter_Associations (Call_Node));

         if Par_Node = Firts_Par_Node then
            Result := True;
         elsif List_Length (Parameter_Associations (Call_Node)) = 2
            and then
             Exclude_Second_Par
         then
            Result := Par_Node = Next (Firts_Par_Node);
         end if;

      end if;

      return Result;
   end Is_Prefix_Notation_Exception;

   ---------------------------------
   -- Is_Protected_Operation_Call --
   ---------------------------------

   function Is_Protected_Operation_Call (Call : Asis.Element) return Boolean is
      Tmp_Node : Node_Id;
      Result   : Boolean := False;
   begin
      Tmp_Node := R_Node (Call);

      if Nkind (Tmp_Node) = N_Entry_Call_Statement then
         Tmp_Node := Prefix (Sinfo.Name (Tmp_Node));
         Tmp_Node := Etype (Tmp_Node);

         if Ekind (Tmp_Node) in Private_Kind then
            Tmp_Node := Full_View (Tmp_Node);
         end if;

         Result := Ekind (Tmp_Node) in Protected_Kind;
      end if;

      return Result;
   end Is_Protected_Operation_Call;

   ------------------------------------
   -- Is_Ref_To_Standard_Num_Subtype --
   ------------------------------------

   function Is_Ref_To_Standard_Num_Subtype
     (Ref  : Asis.Element)
      return Boolean
   is
      Result     : Boolean := False;
      Arg_Entity : Entity_Id;
   begin
      Arg_Entity := Node (Ref);

      if Nkind (Arg_Entity) in N_Has_Entity then

         if No (Entity (Arg_Entity))
           and then
            Nkind (Parent (Arg_Entity)) = N_Expanded_Name
           and then
            Arg_Entity = Selector_Name (Parent (Arg_Entity))
         then
            Arg_Entity := Parent (Arg_Entity);
         end if;

         Arg_Entity := Entity (Arg_Entity);

         if Present (Arg_Entity)
           and then
            Sloc (Arg_Entity) = Standard_Location
           and then
            Ekind (Arg_Entity) in Numeric_Kind
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Is_Ref_To_Standard_Num_Subtype;

   ---------------
   -- Is_Public --
   ---------------

   function Is_Public (Def_Name : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      case Defining_Name_Kind (Def_Name) is
         when A_Defining_Identifier .. A_Defining_Operator_Symbol =>
            Result := not Is_Hidden (Node (Def_Name));
         when A_Defining_Expanded_Name =>
            Result := not Is_Hidden (Node (Defining_Selector (Def_Name)));
         when others =>
            null;
      end case;

      return Result;
   end Is_Public;

   -----------------
   -- Is_Renaming --
   -----------------

   function Is_Renaming (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      --  A very simple test at the moment

      case Flat_Element_Kind (El) is
         when A_Procedure_Renaming_Declaration |
              A_Function_Renaming_Declaration  =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Renaming;

   -------------------------
   -- Is_Standard_Boolean --
   -------------------------

   function Is_Standard_Boolean (Expr : Asis.Element) return Boolean is
      Result      : Boolean := False;
      Type_Entity : Entity_Id;
   begin

      if Asis.Extensions.Is_True_Expression (Expr) then
         Type_Entity := Etype (R_Node (Expr));

         while Present (Type_Entity)
            and then
               Type_Entity /= Etype (Type_Entity)
            and then
               Ekind (Type_Entity) /= E_Enumeration_Type
         loop
            Type_Entity := Etype (Type_Entity);
         end loop;

         Result      := Type_Entity = Standard_Boolean;
      end if;

      return Result;

   end Is_Standard_Boolean;

   ----------------------
   -- Is_Task_Creation --
   ----------------------

   function Is_Task_Creation (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   :          Boolean := False;
   begin

      case Arg_Kind is
         when A_Variable_Declaration |
              A_Constant_Declaration =>
            Result := Is_Task_Object_Declaration (El);
         when A_Single_Task_Declaration =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Task_Creation;

   ------------------------
   -- Is_Task_Entry_Call --
   ------------------------

   function Is_Task_Entry_Call (Call : Asis.Element) return Boolean is
      Pref_Node      : Node_Id;
      Pref_Type_Node : Entity_Id;
      Result         : Boolean   := False;
   begin

      if Statement_Kind (Call) = An_Entry_Call_Statement then
         Pref_Node      := Node (Called_Name (Call));

         if Nkind (Pref_Node) = N_Indexed_Component then
            --  Call to an entry from an entrty family
            Pref_Node := Prefix (Pref_Node);
         end if;

         Pref_Type_Node := Etype (Pref_Node);

         if (No (Pref_Type_Node)
            or else
             Ekind (Pref_Type_Node) = E_Void)
           and then
             Nkind (Pref_Node) = N_Selected_Component
         then
            Pref_Node      := Sinfo.Prefix (Pref_Node);
            Pref_Type_Node := Etype (Pref_Node);
         end if;

         if Present (Pref_Type_Node)
           and then
            Ekind (Pref_Type_Node) in
              E_Private_Type         |
              E_Private_Subtype      |
              E_Limited_Private_Type |
              E_Limited_Private_Subtype
         then
            Pref_Type_Node := Full_View (Pref_Type_Node);
         end if;

         Result := Ekind (Pref_Type_Node) in Task_Kind;
      end if;

      return Result;
   end Is_Task_Entry_Call;

   --------------------------------
   -- Is_Task_Object_Declaration --
   --------------------------------

   function Is_Task_Object_Declaration (Expr : Asis.Element) return Boolean is
      N      : Node_Id;
      Result : Boolean := False;
   begin

      case Flat_Element_Kind (Expr) is
         when A_Variable_Declaration |
              A_Constant_Declaration =>

            N := Defining_Identifier (R_Node (Expr));
            N := Etype (N);

            Result := Ekind (N) in Task_Kind;
         when others =>
            null;
      end case;

      return Result;
   end Is_Task_Object_Declaration;

   ------------------------
   -- Is_Template_Caller --
   ------------------------

   function Is_Template_Caller (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin
      case Flat_Element_Kind (El) is
         when A_Task_Type_Declaration =>
            Result := True;
         when others =>
            null;
      end case;

      return Result;
   end Is_Template_Caller;

   ----------------------------
   -- Is_Unconstrained_Array --
   ----------------------------

   function Is_Unconstrained_Array (Type_Decl : Asis.Element) return Boolean is
      Type_Entity : Entity_Id;
      Result      : Boolean := False;
   begin

      if Declaration_Kind (Type_Decl) = An_Ordinary_Type_Declaration
        or else
         Declaration_Kind (Type_Decl) = A_Subtype_Declaration
      then
         Type_Entity := R_Node (Names (Type_Decl) (1));

         if Is_Array_Type (Type_Entity)
           and then
            not Is_Constrained (Type_Entity)
         then
            Result := True;
         end if;

      end if;

      return Result;

   end Is_Unconstrained_Array;

   --------------------------
   -- Look_For_Loop_Pre_Op --
   --------------------------

   procedure Look_For_Loop_Pre_Op
     (Element :        Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Boolean)
   is
   begin

      case Element_Kind (Element) is
         when A_Statement =>

            case Statement_Kind (Element) is
               when An_If_Statement                    |
                    A_Case_Statement                   |
                    A_Block_Statement                  |
                    An_Extended_Return_Statement       |
                    An_Accept_Statement                |
                    A_Selective_Accept_Statement       |
                    A_Timed_Entry_Call_Statement       |
                    A_Conditional_Entry_Call_Statement |
                    An_Asynchronous_Select_Statement   =>
                  null;
               when A_Loop_Statement       |
                    A_While_Loop_Statement |
                    A_For_Loop_Statement   =>

                  State   := True;
                  Control := Terminate_Immediately;

               when others =>
                  Control := Abandon_Children;
            end case;

         when A_Path =>
            null;
         when others =>
            Control := Abandon_Children;
      end case;

   end Look_For_Loop_Pre_Op;

   ----------------------
   -- Needs_Completion --
   ----------------------

   function Needs_Completion (El : Asis.Element) return Boolean is
      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (El);
      Result   : Boolean                     := False;
      Entity_N : Entity_Id;
   begin

      case Arg_Kind is
         when A_Task_Type_Declaration        |
              A_Protected_Type_Declaration   |
              A_Single_Task_Declaration      |
              A_Single_Protected_Declaration |
              A_Procedure_Body_Stub          |
              A_Function_Body_Stub           |
              A_Package_Body_Stub            |
              A_Task_Body_Stub               |
              A_Protected_Body_Stub          =>
            Result := True;

         when A_Package_Declaration         |
              A_Generic_Package_Declaration =>

            --  Now we make the check for library packages only!

            if Is_Nil (Enclosing_Element (El)) then
               Result :=
                 Asis.Compilation_Units.Is_Body_Required
                   (Enclosing_Compilation_Unit (El));
            end if;

         when A_Generic_Procedure_Declaration |
              A_Generic_Function_Declaration  |
              A_Procedure_Declaration         |
              A_Function_Declaration          =>

            Entity_N := Defining_Unit_Name (Specification (Node (El)));

            if Nkind (Entity_N) = N_Defining_Program_Unit_Name then
               Entity_N := Defining_Identifier (Entity_N);
            end if;

            if not (Is_Intrinsic_Subprogram (Entity_N)
                 or else
                    Is_Imported (Entity_N))
            then
               Result := True;
            end if;

         when others =>
            null;
      end case;

      return Result;
   end Needs_Completion;

   ----------------------
   -- Raises_Exception --
   ----------------------

   function Raises_Exception (El : Asis.Element) return Boolean is
      Result          : Boolean := False;
      First_Handler   : Boolean := Element_Kind (El) = An_Exception_Handler;
      First_Body_Decl : Boolean :=
        Declaration_Kind (El) in
          A_Procedure_Body_Declaration .. A_Function_Body_Declaration;

      procedure Check_Construct
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);
      --  Checks if we have a raise statement or a construct that should be
      --  skipped in the analysis;
      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean);

      procedure Check_For_Raise_Statement is new Traverse_Element
        (Pre_Operation     => Check_Construct,
         Post_Operation    => No_Op,
         State_Information => Boolean);

      Control : Traverse_Control := Continue;

      procedure Check_Construct
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         case Element_Kind (Element) is
            when A_Declaration =>

               case Declaration_Kind (Element) is
                  when A_Procedure_Body_Declaration |
                       A_Function_Body_Declaration  =>

                     if First_Body_Decl then
                        First_Body_Decl := False;
                     else
                        Control := Abandon_Children;
                     end if;

                  when others =>
                     Control := Abandon_Children;
               end case;

            when A_Statement =>
               if Statement_Kind (Element) = A_Raise_Statement then
                  State   := True;
                  Control := Terminate_Immediately;
               end if;
            when A_Path =>
               null;
            when An_Exception_Handler =>
               if First_Handler then
                  First_Handler := False;
               else
                  Control := Abandon_Children;
               end if;

            when others =>
               Control := Abandon_Children;
         end case;
      end Check_Construct;

      procedure No_Op
        (Element :        Asis.Element;
         Control : in out Traverse_Control;
         State   : in out Boolean)
      is
      begin
         null;
      end No_Op;

   begin
      Check_For_Raise_Statement (El, Control, Result);

      return Result;
   end Raises_Exception;

   -------------------------------------
   -- Storage_Order_Defined_By_Pragma --
   -------------------------------------

   function Storage_Order_Defined_By_Pragma
     (E    : Asis.Element)
      return Boolean
   is
      Type_Entity : Entity_Id;
      Next_Pragma : Node_Id;
      Pragma_Arg  : Node_Id;
      Result      : Boolean := False;
   begin
      Type_Entity := R_Node (E);
      Next_Pragma := Next (Type_Entity);
      Type_Entity := Defining_Identifier (Type_Entity);

      while Present (Next_Pragma) loop
         if Nkind (Next_Pragma) = N_Attribute_Definition_Clause
          and then
            Is_Rewrite_Substitution (Next_Pragma)
          and then
            Nkind (Original_Node (Next_Pragma)) = N_Pragma
          and then
            Chars (Next_Pragma) = Name_Scalar_Storage_Order
         then
            Pragma_Arg := Sinfo.Name (Next_Pragma);

            if Nkind (Pragma_Arg) = N_Identifier
              and then
               Entity (Pragma_Arg) = Type_Entity
            then
               Result := True;
               exit;
            end if;
         end if;

         Next_Pragma := Next (Next_Pragma);
      end loop;

      return Result;
   end Storage_Order_Defined_By_Pragma;

   -------------------------------
   -- Used_To_Pass_Actual_Subpr --
   -------------------------------

   function Used_To_Pass_Actual_Subpr (El : Asis.Element) return Boolean is
      Result : Boolean := False;
   begin

      if Declaration_Kind (El) in A_Procedure_Renaming_Declaration ..
        A_Function_Renaming_Declaration
      then
         Result := Pass_Generic_Actual (Node (El));
      end if;

      return Result;
   end Used_To_Pass_Actual_Subpr;

end Gnatcheck.ASIS_Utilities;
