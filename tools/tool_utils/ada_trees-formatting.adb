------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Strings.Fixed;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Indefinite_Vectors;
with ASIS_UL.Vectors;
use type Ada.Containers.Count_Type;
with Text_IO;

with ASIS_UL.Formatted_Output;
with ASIS_UL.Char_Vectors; use ASIS_UL.Char_Vectors;
use ASIS_UL.Char_Vectors.Char_Vectors;
--  use all type ASIS_UL.Char_Vectors.Char_Vector; use all type
--  Ada_Trees.Buffers.Marker_Vector;
with Ada_Trees.Scanner; use Ada_Trees.Scanner.Token_Vectors;

package body Ada_Trees.Formatting is
   use Ada_Trees.Buffers.Marker_Vectors;

   --  Hard and soft line breaks:
   --
   --  A hard line break means a new-line WILL appear in the final output. A
   --  soft line break is a place where a new-line CAN appear; it will appear
   --  only if necessary to make lines short enough. Soft line breaks are
   --  prioritized: if there are several soft line breaks that can be used
   --  to shorten a given line, higher priority ones are chosen over lower
   --  priority ones. Normally, less nested ones are higher priority than
   --  more nested ones.

   type Ada_Template is new W_Str;
   --  This is similar to Formatted_Output.Template, except instead of
   --  inserting strings into the template, it inserts subtrees. See
   --  Interpret_Template in the subunit Tree_To_Ada. The special
   --  characters are:
   --
   --      $ -- insert a hard line break
   --      % -- same as $, but doesn't affect comment indentation
   --           (see Line_Break.Affects_Comments)
   --      { -- indent
   --      } -- outdent
   --      @ -- insert a soft line break. May be followed by 1, 2, etc,
   --           to indicate additional nesting depth.
   --      [ -- continuation-line indent
   --      ] -- continuation-line outdent
   --      ( -- insert a "(", and add "extra" indent by 1 character
   --      ) -- insert a ")", and outdent the "extra"
   --      ^ -- tab based on following token. May be followed by 1, 2, etc,
   --           to indicate Index_In_Line.
   --      & -- insertion point for next "^" tab.
   --      ! -- insert next required subtree
   --      ? -- insert next optional or list subtree
   --      ~ -- delimits arguments of ?
   --      !1, !2, !3, etc -- insert N'th required subtree
   --      ?1, ?2, ?3, etc -- insert N'th optional or list subtree
   --  Other characters are inserted verbatim.
   --
   --  ? takes three arguments, delimited by ~. If the subtree is a list, the
   --  first argument is placed before the list, the second in between list
   --  elements, and the third after the list, except if the list is empty,
   --  nothing is printed. If it's not a list, the first and third arguments
   --  are placed before and after, and the second must be empty, except if
   --  it's Not_An_Element, nothing is printed.
   --
   --  Normally, the soft line breaks inserted by @ have a priority based on
   --  the syntactic nesting depth. Less-nested breaks are enabled in favor of
   --  more deeply nested ones. However, if @ is followed by a digit, that
   --  indicates an additional nesting depth not reflected in the syntax. For
   --  example, if we have "blah @blah @1blah", then the @1 is considered more
   --  nested than the @, so if the line is too long, we first enable the @,
   --  and only enable the @1 if the line is still too long.
   --
   --  Examples:
   --  "!X!X!" -- inserts three subtrees, with "X" in between. "!1X!2X!3" --
   --  same as "!X!X!"
   --
   --  "?(~,~)~" -- a parenthesized comma-separated list
   --
   --  There is no way to escape the special characters, so for example, you
   --  can't print a literal $. So far, that wasn't needed, because those
   --  characters were deliberately chosen not to be part of Ada syntax. They
   --  can of course appear inside string literals and comments, but they're
   --  not needed in the templates.
   --
   --  Pairs of {}, [], and () must match and be properly nested.
   --
   --  The extra indentation for "(" is needed for parenthesized syntax, like
   --  this:
   --
   --      Do_Something
   --        (This,
   --         That);
   --        ^
   --        | Extra blank needed there.
   --
   --  Note: If you want to add new special characters, look at the case
   --  statement in Interpret_Template.

   type Ada_Template_Ptr is access Ada_Template;

   function Handled_Seq (Name_Subtree : Ada_Template) return Ada_Template;
   --  Template for a handled_sequence_of_statements. Name_Subtree is the index
   --  of the subtree that is the defining name, which is used to insert that
   --  name after "end", as in "end Package_Name;".

   function Template_For_Kind (Kind : Ada_Tree_Kind) return Ada_Template_Ptr;

   function L (T1 : Ada_Template) return Ada_Template_Ptr;
   function L (T1, T2 : Ada_Template) return Ada_Template_Ptr;
   function L (T1, T2, T3 : Ada_Template) return Ada_Template_Ptr;
   function L (T1, T2, T3, T4 : Ada_Template) return Ada_Template_Ptr;
   function L (T1, T2, T3, T4, T5 : Ada_Template) return Ada_Template_Ptr;
   function L (T1, T2, T3, T4, T5, T6 : Ada_Template) return Ada_Template_Ptr;
   function L
     (T1, T2, T3, T4, T5, T6, T7 : Ada_Template)
      return                       Ada_Template_Ptr;
   --  8-parameter version not needed so far
   function L
     (T1, T2, T3, T4, T5, T6, T7, T8, T9 : Ada_Template)
      return                               Ada_Template_Ptr;
   --  All the L functions form a template by concatenating together a bunch of
   --  lines.

   function Handled_Seq (Name_Subtree : Ada_Template) return Ada_Template is
   begin
      return "?begin$" &
        "{~;$~;$}~" &
        "?exception$" &
        "{~$~}~" &
        "end?" &
        Name_Subtree &
        " ~~~";
      --  The name after "end" is optional; it is often missing for
      --  block_statements, for example.
   end Handled_Seq;

   Handled_Seq_1 : constant Ada_Template := Handled_Seq ("1");
   Handled_Seq_2 : constant Ada_Template := Handled_Seq ("2");
   Handled_Seq_3 : constant Ada_Template := Handled_Seq ("3");

   Aspects : constant Ada_Template := "? with$" & "{~,$~}~";
   --  ???We could try something like the following: return "? with[@1 ~,@1
   --  ~]~";

   Labels : constant Ada_Template := "?<<~>> <<~>>$~";

   Ret_Typ : constant Ada_Template := "[@1 return? ~~~ !]";

   function L (T1 : Ada_Template) return Ada_Template_Ptr is
   begin
      return new Ada_Template'(T1);
   end L;

   function L (T1, T2 : Ada_Template) return Ada_Template_Ptr is
   begin
      return new Ada_Template'(T1 & T2);
   end L;

   function L (T1, T2, T3 : Ada_Template) return Ada_Template_Ptr is
   begin
      return new Ada_Template'(T1 & T2 & T3);
   end L;

   function L (T1, T2, T3, T4 : Ada_Template) return Ada_Template_Ptr is
   begin
      return new Ada_Template'(T1 & T2 & T3 & T4);
   end L;

   function L (T1, T2, T3, T4, T5 : Ada_Template) return Ada_Template_Ptr is
   begin
      return new Ada_Template'(T1 & T2 & T3 & T4 & T5);
   end L;

   function L
     (T1, T2, T3, T4, T5, T6 : Ada_Template)
      return                   Ada_Template_Ptr
   is
   begin
      return new Ada_Template'(T1 & T2 & T3 & T4 & T5 & T6);
   end L;

   function L
     (T1, T2, T3, T4, T5, T6, T7 : Ada_Template)
      return                       Ada_Template_Ptr
   is
   begin
      return new Ada_Template'(T1 & T2 & T3 & T4 & T5 & T6 & T7);
   end L;

   function L
     (T1, T2, T3, T4, T5, T6, T7, T8, T9 : Ada_Template)
      return                               Ada_Template_Ptr
   is
   begin
      return new Ada_Template'(T1 & T2 & T3 & T4 & T5 & T6 & T7 & T8 & T9);
   end L;

   function Template_For_Kind (Kind : Ada_Tree_Kind) return Ada_Template_Ptr is
   begin
      return
        (case Kind is
           when Def_Names                                              => null,
           when Usage_Names                                            => null,
           when An_Integer_Literal | A_Real_Literal | A_String_Literal => null,

           when Not_An_Element => L (""),

           when Flat_Pragma_Kinds => null,

           when A_Box_Expression => L ("<>"),

           when An_Ordinary_Type_Declaration |
             A_Formal_Type_Declaration =>
             L ("type !! is[@ !", Aspects, "]"),
           when A_Task_Type_Declaration =>
             L ("task type !!",
                Aspects,
                " is? new ~ and ~ with~$",
                "!$",
                "end !1"),
           when A_Protected_Type_Declaration =>
             L ("protected type !!",
                Aspects,
                " is? new ~ and ~ with~$",
                "!$",
                "end !1"),
           when An_Incomplete_Type_Declaration       => L ("type !!", Aspects),
           when A_Formal_Incomplete_Type_Declaration =>
             L ("type !!? is ~~~", Aspects),
           when A_Tagged_Incomplete_Type_Declaration =>
             L ("type !! is tagged", Aspects),
           when A_Private_Type_Declaration      => L ("type !! is !", Aspects),
           when A_Private_Extension_Declaration =>
             L ("type !! is[@ !", Aspects, "]"),
           when A_Subtype_Declaration => L ("subtype ! is[@ !", Aspects, "]"),

           when A_Discriminant_Specification => L ("?~, ~~ :? ~~~ !? := ~~~"),

           when A_Variable_Declaration |
             A_Component_Declaration =>
             L ("?~, ~~ :? ~~~ !? := ~~~", Aspects),

           when A_Return_Variable_Specification =>
             L ("?~, ~~ :? ~~~ !? := ~~~"),
           when A_Return_Constant_Specification =>
             L ("?~, ~~ : constant? ~~~ !? := ~~~"),

           when A_Constant_Declaration =>
             L ("?~, ~~ :? ~~~ constant !? := ~~~", Aspects),
           when A_Deferred_Constant_Declaration =>
             L ("?~, ~~ :? ~~~ constant !", Aspects),
           when A_Single_Task_Declaration =>
             L ("task !", Aspects, " is? new ~~ with~$", "!$", "end !1"),
           when A_Single_Protected_Declaration =>
             L ("protected !", Aspects, " is? new ~~ with~$", "!$", "end !1"),

           when A_Flat_Number_Declaration =>
             L ("?~, ~~ ^: constant ^2:=[@ !]"),

           when An_Enumeration_Literal_Specification => L ("!"),
           when A_Loop_Parameter_Specification |
             A_Generalized_Iterator_Specification =>
             L ("! in[@? ~~~ !]"),
           when An_Element_Iterator_Specification =>
             L ("!? : ~~~ of[@? ~~~ !]"),
           when A_Procedure_Declaration =>
             L ("?~~ ~?~~ ~procedure !? @(~; ~)~? is ~~~", Aspects),
           when A_Function_Declaration =>
             L ("?~~ ~?~~ ~function !? @(~; ~)~" & Ret_Typ & "? is ~~~",
                Aspects),
           when A_Parameter_Specification    => null,
           when A_Procedure_Body_Declaration =>
             L ("?~~ ~?~~ ~procedure !? @(~; ~)~",
                Aspects,
                "@ is$",
                "{?~;$~;$$~}",
                Handled_Seq_3),
           when A_Function_Body_Declaration =>
             L ("?~~ ~?~~ ~function !? @(~; ~)~" & Ret_Typ,
                Aspects,
                "@ is$",
                "{?~;$~;$$~}",
                Handled_Seq_3),

           when A_Null_Procedure_Declaration =>
             L ("?~~ ~?~~ ~procedure !? @(~; ~)~", " is null", Aspects),
           when An_Expression_Function_Declaration =>
             L ("?~~ ~?~~ ~function !? @(~; ~)~" & Ret_Typ & " is[@ !]",
                Aspects),
   --  We don't need parentheses around the "!" after "is", because the
   --  result_expression is always a parenthesized_expression.
           when A_Package_Declaration =>
             L ("package ![@",
                Aspects,
                "]@ is$",
                "?${~;$~};$~",
                "?$private$",
                "{~;$~};$~",
                "end !1"),
           when A_Package_Body_Declaration =>
             L ("package body ![@",
                Aspects,
                "]@ is$",
                "?${~;$~};$$~",
                Handled_Seq_1),
           when An_Object_Renaming_Declaration =>
             L ("?~, ~~ :? ~~~[@ !] renames !", Aspects),
           when An_Exception_Renaming_Declaration =>
             L ("?~, ~~ : exception renames !", Aspects),
           when A_Package_Renaming_Declaration =>
             L ("package ! renames !", Aspects),
           when A_Procedure_Renaming_Declaration =>
             L ("?~~ ~?~~ ~procedure !? @(~; ~)~ renames !", Aspects),
           when A_Function_Renaming_Declaration =>
             L ("?~~ ~?~~ ~function !? @(~; ~)~" & Ret_Typ & " renames !",
                Aspects),
           when A_Generic_Package_Renaming_Declaration =>
             L ("generic package ! renames !", Aspects),
           when A_Generic_Procedure_Renaming_Declaration =>
             L ("generic procedure ! renames !", Aspects),
           when A_Generic_Function_Renaming_Declaration =>
             L ("generic function ! renames !", Aspects),
           when A_Task_Body_Declaration =>
             L ("task body !", Aspects, " is$", "?${~;$~};$$~", Handled_Seq_1),
           when A_Protected_Body_Declaration =>
             L ("protected body !", Aspects, " is$", "?${~;$~};$~", "end !1"),
           when An_Entry_Declaration =>
             L ("?~~ ~?~~ ~entry !?[@ (~~)]~? @(~; ~)~", Aspects),
           when An_Entry_Body_Declaration =>
             L ("entry !?[@ (~~)]~? @(~; ~)~[@ when !@ is]$",
                "?${~;$~};$$~",
                Handled_Seq_1),
           when An_Entry_Index_Specification => L ("for ! in[@ !]"),
           when A_Procedure_Body_Stub        =>
             L ("?~~ ~?~~ ~procedure !? @(~; ~)~ is separate", Aspects),
           when A_Function_Body_Stub =>
             L ("?~~ ~?~~ ~function !? @(~; ~)~" & Ret_Typ & " is separate",
                Aspects),
           when A_Package_Body_Stub =>
             L ("package body ! is separate", Aspects),
           when A_Task_Body_Stub => L ("task body ! is separate", Aspects),
           when A_Protected_Body_Stub =>
             L ("protected body ! is separate", Aspects),
           when An_Exception_Declaration => L ("?~, ~~ : exception", Aspects),
           when A_Choice_Parameter_Specification => L ("!"),
           when A_Generic_Procedure_Declaration  =>
             L ("generic$", "{?~;$~;$~}", "procedure !? @(~; ~)~", Aspects),
           when A_Generic_Function_Declaration =>
             L ("generic$",
                "{?~;$~;$~}",
                "function !? @(~; ~)~" & Ret_Typ,
                Aspects),
           when A_Generic_Package_Declaration =>
             L ("generic$",
                "{?~;$~;$~}",
                "package !",
                Aspects,
                " is$",
                "?${~;$~};$~",
                "?$private$",
                "{~;$~};$~",
                "end !2"),
           when A_Package_Instantiation =>
             L ("package ! is new !? @(~, ~)~", Aspects),
           when A_Procedure_Instantiation =>
             L ("?~~ ~?~~ ~procedure ! is new !? @(~, ~)~", Aspects),
           when A_Function_Instantiation =>
             L ("?~~ ~?~~ ~function ! is new !? @(~, ~)~", Aspects),
           when A_Formal_Object_Declaration    => null,
           when A_Formal_Procedure_Declaration =>
             L ("with procedure !? @(~; ~)~? is ~~~? is ~~~", Aspects),
           when A_Formal_Function_Declaration =>
             L ("with function !? @(~; ~)~" & Ret_Typ & "? is ~~~? is ~~~",
                Aspects),
           when A_Formal_Package_Declaration =>
             L ("with package ! is new !? @(~, ~)~", Aspects),
           when A_Formal_Package_Declaration_With_Box =>
             L ("with package ! is new ! @(<>)", Aspects),
           when A_Derived_Type_Definition => L ("?~~ ~?~~ ~[@new !]"),
           when A_Derived_Record_Extension_Definition =>
             L ("?~~ ~?~~ ~new !? and ~ and ~~ with@ !"),
           when An_Enumeration_Type_Definition   => L ("@(?~,@ ~~)"),
           when A_Signed_Integer_Type_Definition => L ("range !"),
           when A_Modular_Type_Definition        => L ("mod !"),

           when Flat_Root_Type_Kinds =>
             null, -- don't need to print root/universal types

           when A_Floating_Point_Definition => L ("digits !? range ~~~"),
           when An_Ordinary_Fixed_Point_Definition => L ("delta !? range ~~~"),
           when A_Decimal_Fixed_Point_Definition   =>
             L ("delta ! digits !? range ~~~"),
           when A_Constrained_Array_Definition |
             A_Formal_Constrained_Array_Definition =>
             null,
           when An_Unconstrained_Array_Definition |
             A_Formal_Unconstrained_Array_Definition =>
             L ("array[@ (?~ range <>,@ ~ range <>~)] of !"),
           when A_Record_Type_Definition        => L ("?~~ ~?~~ ~!"),
           when A_Tagged_Record_Type_Definition => L ("?~~ ~tagged? ~~~ !"),
           when An_Ordinary_Interface => L ("interface? and ~ and ~~"),
           when A_Limited_Interface => L ("limited interface? and ~ and ~~"),
           when A_Task_Interface => L ("task interface? and ~ and ~~"),
           when A_Protected_Interface           =>
             L ("protected interface? and ~ and ~~"),
           when A_Synchronized_Interface =>
             L ("synchronized interface? and ~ and ~~"),
           when A_Pool_Specific_Access_To_Variable |
             An_Anonymous_Access_To_Variable |
             A_Formal_Pool_Specific_Access_To_Variable =>
             L ("?~~ ~access !"),
           when An_Access_To_Variable |
             A_Formal_Access_To_Variable =>
             L ("?~~ ~access all !"),
           when An_Access_To_Constant |
             An_Anonymous_Access_To_Constant |
             A_Formal_Access_To_Constant =>
             L ("?~~ ~access constant !"),
           when An_Access_To_Procedure |
             An_Anonymous_Access_To_Procedure |
             A_Formal_Access_To_Procedure =>
             L ("?~~ ~access procedure? @(~; ~)~"),
           when An_Access_To_Protected_Procedure |
             An_Anonymous_Access_To_Protected_Procedure |
             A_Formal_Access_To_Protected_Procedure =>
             L ("?~~ ~access protected procedure? @(~; ~)~"),
           when An_Access_To_Function |
             An_Anonymous_Access_To_Function |
             A_Formal_Access_To_Function =>
             L ("?~~ ~access function? @(~; ~)~" & Ret_Typ),
           when An_Access_To_Protected_Function |
             An_Anonymous_Access_To_Protected_Function |
             A_Formal_Access_To_Protected_Function =>
             L ("?~~ ~access protected function? @(~; ~)~" & Ret_Typ),
           when A_Subtype_Indication => null,
           when A_Discrete_Subtype_Indication_As_Subtype_Definition |
             A_Discrete_Subtype_Indication =>
             L ("!? range ~~~"),
           when A_Range_Attribute_Reference => L ("!"),
           when A_Discrete_Range_Attribute_Reference_As_Subtype_Definition |
             A_Discrete_Range_Attribute_Reference =>
             L ("!"),
           when A_Simple_Expression_Range |
             A_Discrete_Simple_Expression_Range_As_Subtype_Definition |
             A_Discrete_Simple_Expression_Range =>
             L ("[@! ..[@ !]]"),
           when A_Digits_Constraint => L ("digits !? range ~~~"),
           when A_Delta_Constraint  => L ("delta !? range ~~~"),
           when An_Index_Constraint | A_Discriminant_Constraint =>
             L ("?@(~,@ ~)~"),
           when A_Component_Definition       => L ("?~~ ~!"),
           when An_Unknown_Discriminant_Part => L (" @(<>)"),
           when A_Known_Discriminant_Part    => L ("? @(~; ~)~@"),
           when A_Record_Definition          =>
             L ("? ~~~", "record$", "{?~;$~;$~}", "end record"),
           when A_Null_Record_Definition         => L ("null record"),
           when A_Null_Component                 => L ("null"),
           when A_Variant_Part => L ("case ! is$", "{!}", "end case"),
           when An_Others_Choice                 => L ("others"),
           when A_Private_Type_Definition        => L ("?~~ ~?~~ ~private"),
           when A_Tagged_Private_Type_Definition =>
             L ("?~~ ~tagged? ~~~ private"),
           when A_Private_Extension_Definition =>
             L ("?~~ ~?~~ ~?~~ ~new !? and ~ and ~~ with private"),
           when A_Task_Definition |
             A_Protected_Definition =>
             L ("?${~;$~};$~", "?$private$", "{~;$~};$~"),
           when A_Formal_Private_Type_Definition => L ("?~~ ~?~~ ~private"),
           when A_Formal_Tagged_Private_Type_Definition =>
             L ("?~~ ~tagged? ~~~ private"),
           when A_Formal_Derived_Type_Definition =>
             L ("?~~ ~?~~ ~?~~ ~new !? and ~ and ~~? with ~~~"),
           when A_Formal_Discrete_Type_Definition        => L ("@(<>)"),
           when A_Formal_Signed_Integer_Type_Definition  => L ("range <>"),
           when A_Formal_Modular_Type_Definition         => L ("mod <>"),
           when A_Formal_Floating_Point_Definition       => L ("digits <>"),
           when A_Formal_Ordinary_Fixed_Point_Definition => L ("delta <>"),
           when A_Formal_Decimal_Fixed_Point_Definition  =>
             L ("delta <> digits <>"),
           when A_Formal_Ordinary_Interface => L ("interface? and ~ and ~~"),
           when A_Formal_Limited_Interface  =>
             L ("limited interface? and ~ and ~~"),
           when A_Formal_Task_Interface => L ("task interface? and ~ and ~~"),
           when A_Formal_Protected_Interface =>
             L ("protected interface? and ~ and ~~"),
           when A_Formal_Synchronized_Interface =>
             L ("synchronized interface? and ~ and ~~"),
           when An_Aspect_Specification => L ("!? => ~~~"),

           when An_Explicit_Dereference => L ("!.all"),
           when A_Function_Call         => L ("!? @(~, ~)~!!"),
   --  The last two !'s are Is_Prefix_Call and Is_Prefix_Notation, which don't
   --  generate anything.
           when An_Indexed_Component => L ("!? @(~, ~)~"),
           when A_Slice              => L ("![@ (!)]"),
           when A_Selected_Component => L ("![@.!]"),

           when Flat_Attribute_Reference_Kinds =>
             (if Kind in Flat_Attr_Ref_Without_Exp_Kinds then L ("!'[@!]")
              elsif
                Kind in Flat_Attr_Ref_With_Exp_Kinds
              then
                L ("!'[@!? @(~, ~)~]")
              else L ("? ? ?")), -- can't happen

           when A_Record_Aggregate |
             A_Positional_Array_Aggregate |
             A_Named_Array_Aggregate =>
             L ("?@(~,@ ~)~"),
           when An_Extension_Aggregate => L ("@(! with @", "?~,@ ~~)"),
           when An_And_Then_Short_Circuit | An_Or_Else_Short_Circuit => null,
           when An_In_Membership_Test => L ("! in[@ ?[@~ |@ ~]~]"),
           when A_Not_In_Membership_Test => L ("! not in[@ ?[@~ |@ ~]~]"),
           when A_Null_Literal => L ("null"),
           when A_Parenthesized_Expression => L ("@(!)"),
           when A_Raise_Expression => L ("raise !?[@ with ~~]~"),
           when A_Type_Conversion => L ("![@ (!)]"),
           when A_Qualified_Expression => L ("!'[@(!)]"),
           when An_Allocation_From_Subtype |
             An_Allocation_From_Qualified_Expression =>
             L ("new? @(~~)~ !"),
           when A_Case_Expression => L ("case ! is[@ ?@~,@ ~~]"),
           when An_If_Expression                 => L ("?~@ ~~"),
           when A_For_All_Quantified_Expression  => L ("for all ! => !"),
           when A_For_Some_Quantified_Expression => L ("for some ! => !"),
           when A_Pragma_Argument_Association |
             A_Discriminant_Association |
             A_Record_Component_Association |
             An_Array_Component_Association |
             A_Parameter_Association |
             A_Generic_Association =>
             null,
           when A_Null_Statement        => L (Labels, "null"),
           when An_Assignment_Statement => L (Labels, "! ^:=[@ !]"),
           when An_If_Statement         => L (Labels, "!", "end if"),
           when A_Case_Statement        =>
             L (Labels, "case !@ is$", "{!}", "end case"),
           when A_Loop_Statement =>
             L (Labels, "?~~ :$~loop$", "{?~;$~;$~}", "end loop?2 ~~~"),
           when A_While_Loop_Statement =>
             L (Labels,
                "?~~ :$~while[ !]@ loop$",
                "{?~;$~;$~}",
                "end loop?2 ~~~"),
           when A_For_Loop_Statement =>
             L (Labels, "?~~ :$~for !@ loop$", "{?~;$~;$~}", "end loop?2 ~~~"),
           when A_Block_Statement          => null,
           when An_Exit_Statement => L (Labels, "exit? ~~~? when[ ~~]~"),
           when A_Goto_Statement           => L (Labels, "goto !"),
           when A_Procedure_Call_Statement => L (Labels, "!? @(~, ~)~!"),
   --  The last ! is Is_Prefix_Notation, which doesn't generate anything.
           when An_Entry_Call_Statement      => L (Labels, "!? @(~, ~)~"),
           when A_Return_Statement           => L (Labels, "return[? ~~~]"),
           when An_Extended_Return_Statement =>
             L (Labels, "return[@ !]@ do$",
                "{?~;$~;$~}",
                "?exception$",
                "{~$~}~",
                "end return"),
           when An_Accept_Statement =>
             L (Labels,
                "accept !? @(~~)~? @(~; ~)~ do$",
                "{?~;$~;$~}",
                "?exception$",
                "{~$~}~",
                "end !2"),
           when A_Requeue_Statement            => L (Labels, "requeue !"),
           when A_Requeue_Statement_With_Abort =>
             L (Labels, "requeue ! with abort"),
           when A_Delay_Until_Statement => L (Labels, "delay until !"),
           when A_Delay_Relative_Statement        => L (Labels, "delay !"),
           when A_Terminate_Alternative_Statement => L ("terminate"),
           when A_Selective_Accept_Statement |
             A_Timed_Entry_Call_Statement |
             A_Conditional_Entry_Call_Statement |
             An_Asynchronous_Select_Statement =>
             L (Labels, "select", "!", "end select"),
           when An_Abort_Statement       => L (Labels, "abort ?~, ~~"),
           when A_Raise_Statement => L (Labels, "raise? ~~~?[@ with ~~]~"),
           when A_Code_Statement         => null,
           when An_If_Path               => L ("if[ !]@ then$", "{?~;$~;$~}"),
           when An_If_Expression_Path    => L ("if[@ !]@ then[@ !]"),
           when An_Elsif_Path => L ("elsif[ !]@ then$", "{?~;$~;$~}"),
           when An_Elsif_Expression_Path => L ("elsif[@ !]@ then[@ !]"),
           when An_Else_Path             => L ("else$", "{?~;$~;$~}"),
           when An_Else_Expression_Path  => L ("else[@ !]"),
           when A_Case_Path |
             A_Variant =>
             L ("when[ ?~ ^|@ ~~] =>$", "{?~;$~;$~}"),
           when A_Select_Path          => L ("? when ~~ =>~$", "{?~;$~;$~}"),
           when An_Or_Path             => L ("or? when ~~ =>~$", "{?~;$~;$~}"),
           when A_Then_Abort_Path      => L ("then abort$", "{?~;$~;$~}"),
           when A_Case_Expression_Path => L ("when[ ?~ |@ ~~] =>[@ !]"),
           when A_Use_Package_Clause   => L ("use ?~, ~~"),
           when A_Use_Type_Clause      => L ("use type ?~, ~~"),
           when A_Use_All_Type_Clause  => L ("use all type[@ ?~,@ ~~]"),
           when A_With_Clause          => L ("?~~ ~?~~ ~with ^?~, ~~"),
   --  Note: the tab ('^') is ignored for limited/private 'with's (see
   --  Append_Tab).
           when An_Attribute_Definition_Clause       => L ("for ! use !"),
           when An_Enumeration_Representation_Clause => L ("for ! use !"),
           when A_Record_Representation_Clause       =>
             L ("for ! use record? at mod ~~;~$", "{?~;$~;$~}", "end record"),
           when An_At_Clause         => L ("for ! use at !"),
           when A_Component_Clause   => null,
           when An_Exception_Handler =>
             L ("when? ~~ :~ ?~ | ~~ =>$", "{?~;$~;$~}"),
           when A_Comment => null,
           when An_Aliased                           => L ("aliased"),
           when A_Null_Exclusion | A_Not_Null_Return => L ("not null"),
           when A_Reverse                            => L ("reverse"),
           when A_Limited                            => L ("limited"),
           when A_Synchronized                       => L ("synchronized"),
           when A_Private                            => L ("private"),
           when An_Abstract                          => L ("abstract"),
           when A_Tagged                             => L ("tagged"),
           when An_Overriding                        => L ("overriding"),
           when A_Not_Overriding                     => L ("not overriding"),
           when An_Is_Prefix_Call                    => L (""),
   --  Generate nothing; Is_Prefix_Call is handled elsewhere
           when An_Is_Prefix_Notation => L (""),
   --  Generate nothing; Is_Prefix_Notation is handled elsewhere
           when A_Compilation_Unit => null,
           when Flat_List_Kinds    => null);

   end Template_For_Kind;

   type Template_Table_Type is array (Ada_Tree_Kind) of Ada_Template_Ptr;

   Template_Table             : Template_Table_Type;
   Template_Table_Initialized : Boolean := False;

   ----------------

   Out_Buf : Buffer;
   --  Buffer containing the text that we will eventually output as the final
   --  result. We first fill this with initially formatted text by walking the
   --  tree, and then we modify it repeatedly in multiple passes.

   type Nesting_Level is new Natural;

   Cur_Indentation : Natural := 0;

   Next_Line_Break_Unique_Id : Modular := 1;
   --  Used to set Line_Break.UID for debugging.

   --  Each line break is represented by a Line_Break appended onto the
   --  Line_Breaks vector. Hard line breaks are initially enabled. Soft line
   --  breaks are initially disabled, and will be enabled if necessary to make
   --  lines short enough.

   type Line_Break is record
      Mark : Marker;
      --  Marks the (potential) line break in the buffer. For a hard line
      --  break, there is an NL character at that position. For a soft one,
      --  there is initially nothing in the buffer; an NL will be inserted
      --  at Mark if the line break becomes enabled.
      --
      --  The reason for inserting NL characters is so we can call Get_Tokens
      --  on the buffer. The reason for not doing so for soft line breaks
      --  is that it's not necessary (there will always be something to
      --  prevent two tokens running together), and it makes the line
      --  length calculation simpler.

      Hard : Boolean;
      --  True for a hard line break, False for a soft one
      Affects_Comments : Boolean;
      --  True if the indentation of this Line_Break should affect the
      --  indentation of surrounding comments. For example, True for '$' but
      --  False for '%" (see type Ada_Template).
      Enabled : Boolean;
      --  True if this line break will appear in the final output
      Level : Nesting_Level := 1000;
      --  Nesting level of [...] (continuation-line indentation, mainly for
      --  soft line breaks).
      Indentation : Natural := 1000;
      --  Indentation level of this line break
      Length : Natural := Natural'Last;
      --  Number of characters in line, not counting NL. Calculated by
      --  Split_Lines. Valid only for enabled line breaks.

      --  For debugging:

      Kind     : Ada_Tree_Kind;
      Template : Name_Id;
      UID      : Modular := 123_456_789;
   end record; -- Line_Break

   type Line_Break_Index is new Positive;
   type Line_Break_Array is array (Line_Break_Index range <>) of Line_Break;
   package Line_Break_Vectors is new ASIS_UL.Vectors
     (Line_Break_Index,
      Line_Break,
      Line_Break_Array);
   subtype Line_Break_Vector is Line_Break_Vectors.Vector;
   use Line_Break_Vectors;
   --  use all type Line_Break_Vector;

   All_Line_Breaks : Line_Break_Vector;
   --  All line breaks in the whole input file. Built in two passes.

   Temp_Line_Breaks : Line_Break_Vector;
   --  Used by Insert_Comments_And_Blank_Lines to add new line breaks to
   --  All_Line_Breaks; they are appended to Temp_Line_Breaks, which is then
   --  merged with All_Line_Breaks when done. This is for efficiency and to
   --  keep the tables in source-location order.

   Enabled_Line_Breaks : Line_Break_Vector;
   --  All enabled line breaks
   Syntax_Line_Breaks : Line_Break_Vector;
   --  All (enabled) nonblank hard line breaks. These are called "Syntax_..."
   --  because they are determined by the syntax (e.g. we always put a line
   --  break after a statement).

   --  ???Perhaps make the above tables contain Line_Break_Indexes instead of
   --  Line_Breaks. Can we use an index into a single table instead of UID?

   procedure Collect_Enabled_Line_Breaks (Syntax_Also : Boolean);
   --  Collect all the enabled line breaks, and (if Syntax_Also is True) also
   --  the syntax line breaks.

   procedure Collect_Enabled_Line_Breaks (Syntax_Also : Boolean) is
   begin
      Clear (Enabled_Line_Breaks);
      Clear (Syntax_Line_Breaks);

      --  We always include the last one, even though it has Length = 0

      for J in 1 .. Last_Index (All_Line_Breaks) loop
         if All_Line_Breaks (J).Enabled then
            Append (Enabled_Line_Breaks, All_Line_Breaks (J));
            if Syntax_Also and then All_Line_Breaks (J).Hard then
               if All_Line_Breaks (J).Length /= 0
                 or else J = Last_Index (All_Line_Breaks)
               then
                  Append (Syntax_Line_Breaks, All_Line_Breaks (J));
               end if;
            end if;
         end if;
      end loop;
   end Collect_Enabled_Line_Breaks;

   function Line_Break_LT (X, Y : Line_Break) return Boolean;

   function Line_Break_LT (X, Y : Line_Break) return Boolean is
   begin
      return Mark_LT (Out_Buf, X.Mark, Y.Mark);
   end Line_Break_LT;

   package Line_Break_Sorting is new Line_Break_Vectors.Generic_Sorting
     ("<" => Line_Break_LT);

   ------------------------
   -- Tabs and Alignment --
   ------------------------

   --  We use "tabs" to implement alignment. For example, if the input is:
   --     X : Integer := 123;
   --     Long_Ident : Boolean := False;
   --     Y : constant Long_Type_Name := Something;
   --  we're going to align the ":" and ":=" in the output, like this:
   --     X          : Integer                 := 123;
   --     Long_Ident : Boolean                 := False;
   --     Y          : constant Long_Type_Name := Something;
   --
   --  A "tab" appears before each ":" and ":=" in the above. This information
   --  is recorded in Tabs, below. The position of the tab in the buffer
   --  is indicated by Mark, which gets automatically updated as unrelated
   --  passes update Out_Buf. Finally, Insert_Alignment calculates the Col
   --  and Num_Blanks for each tab, and then inserts blanks accordingly.
   --
   --  A tab always occurs at the start of a token.

   type Tab_Index_In_Line is range 1 .. 9;
   --  We probably never have more than a few tabs in a given construct, so 9
   --  should be plenty, and it allows us to use a single digit in the
   --  templates, as in "^2".

   type Tab_Rec is record
      Parent, Tree : Ada_Tree_Base;
      --  Tree is the tree whose template generated this tab, and Parent is its
      --  parent. Tree is used to ensure that the relevant tabs within a single
      --  line all come from the same tree; other tabs in the line are ignored.
      --  Parent is used across lines to ensure that all lines within a
      --  paragraph to be aligned together all come from the same parent tree.
      Token : Name_Id := Name_Find ("");
      --  This is some text associated with the Tab. Usually, it is the text of
      --  the token that follows the Tab in the template.
      Mark : Marker;
      --  Position in the buffer of the tab
      Index_In_Line   : Tab_Index_In_Line := Tab_Index_In_Line'Last;
      Col             : Positive          := Positive'Last;
      --  Column number of the tab
      Num_Blanks : Natural := 0;
      --  Number of blanks this tab should expand into
      Is_Fake : Boolean;
      --  True if this is a "fake tab", which means that it doesn't actually
      --  insert any blanks (Num_Blanks = 0). See Append_Tab for more
      --  explanation.
      Is_Insertion_Point : Boolean;
      --  False for "^", true for "&". Normally, "^" means insert blanks at the
      --  point of the "^" to align things. However, if there is a preceding
      --  (and matching) "&", then the blanks are inserted at the "insertion
      --  point" indicated by "&". This feature provides for
      --  right-justification.
      --  See Tree_To_Ada.Insert_Alignment.Calculate_Num_Blanks.Process_Line in
      --  ada_trees-formatting-tree_to_ada.adb for more information.
   end record;

   type Tab_Index is new Positive;
   type Tab_Array is array (Tab_Index range <>) of Tab_Rec;
   package Tab_Vectors is new ASIS_UL.Vectors (Tab_Index, Tab_Rec, Tab_Array);
   subtype Tab_Vector is Tab_Vectors.Vector;
   use Tab_Vectors;
   --  use all type Tab_Vector;

   Tabs : Tab_Vector;
   --  All of the tabs in the whole input file, in increasing order

   package Tab_In_Line_Vectors is new Ada.Containers.Bounded_Vectors
     (Tab_Index_In_Line,
      Tab_Index);
   use Tab_In_Line_Vectors;
   subtype Tab_In_Line_Vector is
     Tab_In_Line_Vectors
       .Vector
     (Capacity => Ada.Containers.Count_Type (Tab_Index_In_Line'Last));

   type Tab_In_Line_Vector_Index is new Positive;
   package Tab_In_Line_Vector_Vectors is new Ada.Containers.Indefinite_Vectors
     (Tab_In_Line_Vector_Index,
      Tab_In_Line_Vector);
   use Tab_In_Line_Vector_Vectors;
   --  We use Indefinite_Vectors rather than Vectors because otherwise we get
   --  "discriminant check failed" at a-cobove.ads:371. I'm not sure whether
   --  that's a compiler bug.

   Src_Tokens, -- from original source file (Src_Buf)
   Out_Tokens : -- from Out_Buf
   Scanner.Token_Vector;

   Out_Buf_Line_Ends : aliased Marker_Vector;

   function Next_Enabled (F : Line_Break_Index) return Line_Break_Index;
   --  Next currently-enabled line break after F. Thus, F..Next_Enabled(F) is a
   --  line.

   function Next_Enabled (F : Line_Break_Index) return Line_Break_Index is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
      First : constant Line_Break := Line_Breaks (F);
      pragma Assert (First.Enabled);
      Result : Line_Break_Index := F + 1;
      Last   : Line_Break       := Line_Breaks (Result);

   begin
      while not Last.Enabled loop
         Result := Result + 1;
         Last   := Line_Breaks (Result);
      end loop;

--???      pragma Assert (First.Level = Last.Level);
      return Result;
   end Next_Enabled;

   function Is_Empty_Line (F, L : Line_Break_Index) return Boolean;
   --  True if F..L forms an empty line (or would, if both were enabled).

   function Line_Text (F, L : Line_Break_Index) return W_Str;
   --  F and L are the first and last index forming a line; returns the text of
   --  the line, not including any new-lines.

   function Is_Empty_Line (F, L : Line_Break_Index) return Boolean is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
      First : constant Line_Break := Line_Breaks (F);
      Last  : constant Line_Break := Line_Breaks (L);
      FP    : Positive            := Position (Out_Buf, First.Mark);
      LP    : constant Positive   := Position (Out_Buf, Last.Mark);

      procedure Assert (Result : Boolean);
      procedure Assert (Result : Boolean) is
         S : constant W_Str := Slice (Out_Buf, FP, LP - 1);
      begin
         if Result then
            pragma Assert (S = "");
         else
            pragma Assert (S /= "");
            pragma Assert (S /= " ");
            if True then -- slow
               pragma Assert (for some C of S => C /= ' ');
            end if;
         end if;
      end Assert;

   begin
      --  Take into account the fact that a hard line break occupies one
      --  character (the NL), whereas a soft line break does not, and the fact
      --  that a soft line break can be preceded or followed by a single blank
      --  (but not both).

      if First.Hard then
         FP := FP + 1;
      end if;
      if FP < LP and then Char_At (Out_Buf, FP) = ' ' then
         FP := FP + 1;
      end if;
      pragma Assert (FP <= LP);

      return Result : constant Boolean := FP = LP do
         pragma Debug (Assert (Result));
      end return;
   end Is_Empty_Line;

   function Line_Text (F, L : Line_Break_Index) return W_Str is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
      First  : constant Line_Break := Line_Breaks (F);
      Last   : constant Line_Break := Line_Breaks (L);
      Result : constant W_Str      := Slice (Out_Buf, First.Mark, Last.Mark);

   begin
      return Result (Result'First + 1 .. Result'Last);
   end Line_Text;

   function Tab_Image (X : Tab_Index) return String;

   function Tab_Image (X : Tab_Index) return String is
      Tab : constant Tab_Rec := Tabs (X);

   begin
      return "Tabs(" &
        Image (Integer (X)) &
        ") = ^" &
        Image (Integer (Tab.Index_In_Line)) &
        Get_Name_String (Tab.Token) &
        ASCII.HT &
        " at " &
        Image (Position (Out_Buf, Tab.Mark)) &
        (if Tab.Col = Positive'Last then "" else " Col = " & Image (Tab.Col)) &
        (if Tab.Num_Blanks = 0 then ""
         else " Blanks = " & Image (Tab.Num_Blanks)) &
        (if Tab.Is_Fake then " FAKE" else "") &
        (if Tab.Tree = null then "" else "(Tr = " & T_Img (Tab.Tree) & ")") &
        (if Tab.Parent = null then ""
         else "(Pa = " & T_Img (Tab.Parent) & ")");
   end Tab_Image;

   -------------------------------------
   -- Support for -pp-off and --pp-on --
   -------------------------------------

   Pp_Off_On_Delimiters : Scanner.Pp_Off_On_Delimiters_Rec;

   procedure Tree_To_Ada
     (Root      : Ada_Tree;
      Src_Buf   : in out Buffer;
      Write_BOM : Boolean;
      Options   : Formatting_Options;
      Output_Name : String;
      Form_String : String;
      Do_Diff : Boolean;
      Output_Written : out Boolean) is separate;

   procedure Tree_To_Ada
     (Root        : Ada_Tree;
      Source_Name : String;
      Options     : Formatting_Options;
      Output_Name : String)
   is
      Src_Buf : Buffer;
      --  Buffer containing the text of the original source file

      Write_BOM : Boolean;
      --  True if BOM should be written to the output

      Output_Written : Boolean := False;
   begin
      Read_Ada_File
        (Src_Buf, Source_Name, BOM_Seen => Write_BOM, Expand_Tabs => True);
      Tree_To_Ada (Root, Src_Buf, Write_BOM, Options, Output_Name,
        Form_String => "", Do_Diff => False, Output_Written => Output_Written);
      pragma Assert (Output_Written);
   end Tree_To_Ada;

   procedure Put_Line_Breaks (Line_Breaks : Line_Break_Vector);
   --  ???This doesn't work unless Line_Breaks is All_Line_Breaks, because of
   --  various global variables!

   procedure Put_Line_Break (Break : Line_Break);

   procedure Put_Buf_With_Marks;

   procedure Put_Buf_With_Marks is
      Line_Breaks : Line_Break_Vector renames All_Line_Breaks;
      Cur_Line : Line_Break_Index := 1;
      Cur_Tab  : Tab_Index        := 1;
      S        : constant String  := To_String (Out_Buf);

      use Dbg_Out;

   begin
      if not Dbg_Out.Output_Enabled then
         return;
      end if;

      for Cur_Char in S'Range loop
         while Cur_Tab <= Last_Index (Tabs)
           and then Position (Out_Buf, Tabs (Cur_Tab).Mark) = Cur_Char
         loop
            Put_Char ('^');
            Cur_Tab := Cur_Tab + 1;
         end loop;

         declare
            Indentation : Natural := 0;
         begin
            while Cur_Line <= Last_Index (Line_Breaks)
              and then
                Position (Out_Buf, Line_Breaks (Cur_Line).Mark) =
                Cur_Char
            loop
               declare
                  Break : constant Line_Break := Line_Breaks (Cur_Line);
               begin
                  if Break.Enabled then
                     Indentation := Break.Indentation;
                  end if;

                  if Break.Hard then
                     Put_Char ('$');

                  elsif Break.Enabled then
                     Put_Char ('!');

                  else
                     Put_Char ('?');
                  end if;
                  Put ("\1", Image (Integer (Break.Level)));
               end;
               Cur_Line := Cur_Line + 1;
            end loop;

            Put_Char (S (Cur_Char));

            for J in 1 .. Indentation loop
               Put_Char ('_');
            end loop;
         end;
      end loop;
   end Put_Buf_With_Marks;

   procedure Put_Line_Break (Break : Line_Break) is
      use Dbg_Out;
   begin
      Put
        ("\1\2, \3, \4, \5\n",
         String'(1 .. Break.Indentation => '_'),
         Image (Position (Out_Buf, Break.Mark)),
         (if Break.Enabled then "hard" else "soft"),
         (if Break.Enabled then "enabled" else "disabled"),
         (if At_Point (Out_Buf, Break.Mark) then "at point"
          elsif
            Position (Out_Buf, Break.Mark) = Point (Out_Buf) - 1
          then
            "just before point"
          else "not at point"));
   end Put_Line_Break;

   procedure Put_Line_Breaks (Line_Breaks : Line_Break_Vector) is
      L        : Line_Break_Index;
      Line_Num : Natural := 0; -- only counts enabled lines
      use Dbg_Out;
   begin
      Put
        ("Last_Index (Line_Breaks) = \1\n",
         Image (Integer (Last_Index (Line_Breaks))));

      for Cur_Line in 1 .. Last_Index (Line_Breaks) loop
         if Line_Breaks (Cur_Line).Enabled then
            Line_Num := Line_Num + 1;
         end if;

         Put
           ("\1:\t\2\3\4",
            Image (Line_Num),
            String'(1 .. Line_Breaks (Cur_Line).Indentation => '_'),
            Image (Position (Out_Buf, Line_Breaks (Cur_Line).Mark)),
            (if Line_Breaks (Cur_Line).Enabled then "" else "?"));

         Put (" lev=\1", Image (Integer (Line_Breaks (Cur_Line).Level)));

         if False then
            Put ("\t\1", Image (Line_Breaks (Cur_Line).Kind));
            Put ("\t\1", Get_Name_String (Line_Breaks (Cur_Line).Template));
         end if;

         if Line_Breaks (Cur_Line).Enabled
           and then Cur_Line /= Last_Index (Line_Breaks)
         then
            L := Next_Enabled (Cur_Line);
            Put
              ("\t\1..\2 len=\3",
               Image (Integer (Cur_Line)),
               Image (Integer (L)),
               Image (Line_Breaks (Cur_Line).Length));
            Put ("\t<<\1>>", To_UTF8 (Line_Text (Cur_Line, L)));
         end if;

         Put ("#\1", Image (Line_Breaks (Cur_Line).UID));
         Put ("\n");
      end loop;
      for Cur_Line in 1 .. Last_Index (Line_Breaks) loop
         Put_Line_Break (Line_Breaks (Cur_Line));
      end loop;
   end Put_Line_Breaks;

   procedure Format_Debug_Output (Message : String) is
      use Dbg_Out;
   begin
      if not Dbg_Out.Output_Enabled then
         return;
      end if;

      Text_IO.Flush (Text_IO.Standard_Output);
      Text_IO.Flush (Text_IO.Standard_Error);

      Put ("\n\nFormat_Debug_Output: \1:\n", Message);

      Dump_Buf (Out_Buf);

      Put_Line_Breaks (All_Line_Breaks);

      for X in 1 .. Last_Index (Tabs) loop
         Put ("\1\n", Tab_Image (X));
      end loop;

      Put_Buf_With_Marks;

      Text_IO.Flush (Text_IO.Standard_Error);
      Text_IO.Flush (Text_IO.Standard_Output);
   end Format_Debug_Output;

   procedure Put_Ada_Templates is
      use Formatted_Output, Ada.Strings.Fixed;

   begin
      Put ("--  Templates:\n");

      for Kind in Ada_Tree_Kind loop
         if Template_Table (Kind) /= null then
            declare
               T : constant String :=
                 To_UTF8 (W_Str (Template_Table (Kind).all));

            begin
               Put
                 ("--  \1 => \2",
                  Image (Kind),
                  (if Template_Table (Kind) = null then "null"
                   else """" & T & """"));
               if Count (T, "[") /= Count (T, "]") then
                  Put ("    MISMATCHED [...]");
                  raise Program_Error;
               end if;
               if Count (T, "{") /= Count (T, "}") then
                  Put ("    MISMATCHED {...}");
                  raise Program_Error;
               end if;
               if Count (T, "(") /= Count (T, ")") then
                  Put ("    MISMATCHED (...)");
                  raise Program_Error;
               end if;
               Put ("\n");
            end;
         end if;
      end loop;
      Put ("--  End templates.\n");
   end Put_Ada_Templates;

end Ada_Trees.Formatting;
