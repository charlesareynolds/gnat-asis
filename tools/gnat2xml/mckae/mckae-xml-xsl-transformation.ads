------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2007 McKae Technologies                    --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package McKae.XML.XSL.Transformation is

   --  Failure in transformation

   type Application_Statuses is
     (Success, -- The XSL transformation was successfully performed
     Bad_Input_File, -- The input file could not be opened/read
     Input_Not_Xml, -- The input file does not contain valid XML
     Bad_Output_File, -- The output file could not be created/written
     Save_Failed, -- Saving the transformed file failed
     Bad_Xsl_File, -- The XSL file count not be opened/read
     Xsl_Not_Xsl, -- The XSL file does not contain valid XSL
     Transformation_Failed -- The application of the XSL stylesheet failed
     );

   --  Association of keys and values to provide to an XSL stylesheet's parameter
   --  elements.

   type Key_Value_Pairs is record
      Key   : Unbounded_String;
      Value : Unbounded_String;
   end record;

   --  Index range for parameters

   type Parameter_Indices is new Natural;

   --  List of parameter settings to be provided

   type Parameter_Settings is
     array (Parameter_Indices range <>) of Key_Value_Pairs;

   No_Parameters : constant Parameter_Settings (1 .. 0) :=
     (others => (Null_Unbounded_String, Null_Unbounded_String));

   --  Apply the XSL stylesheet to the given file, writing the transformed XML
   --  into the provided file.
   procedure Apply_Xsl
     (Input_Filename : String;
   --  Filename containing the XML to be transformed

      Xsl_Filename : String;
   --  The file containing the XSL to apply to the input file

      Target_Filename : String;
   --  The file into which to write the result of the transformation

      Status : out Application_Statuses;
   --  Status of the tranformation application

      Parameters : Parameter_Settings := No_Parameters
   --  Parameters to be provided to the stylesheet
   );

end McKae.XML.XSL.Transformation;
