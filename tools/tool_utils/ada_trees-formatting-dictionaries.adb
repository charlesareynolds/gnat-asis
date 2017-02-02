------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--                  G N A T P P . D I C T I O N A R I E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2001-2013, AdaCore                      --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Alloc;
with Table;
with Types; use Types;

package body Ada_Trees.Formatting.Dictionaries is

   --  For dictionaries, we are copying the GNAT Name Table structure. We have
   --  one dictionary for the whole words and for substrings

   --  Note, that all the dictionaries are based on the plain String type,
   --  whereas ASIS uses Program_Text (=Wide_String) for images.

   package Name_Chars is new Table.Table
     (Table_Component_Type => Character,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 0,
      Table_Initial        => Alloc.Name_Chars_Initial,
      Table_Increment      => Alloc.Name_Chars_Increment,
      Table_Name           => "PP_Name_Chars");

   type String_Id is new Name_Id;
   No_String       : constant String_Id := String_Id (No_Name);
   First_String_Id : constant String_Id := String_Id (First_Name_Id);

   type Casing_Exception_Kinds is
     (Not_A_Casing_Exception,  -- Wrong syntax of the exception string
     Whole_Word,              --  name to be replaced as a whole
     Subword);

   --  Subword is a part of the name delimited by '_' or by the beginning or
   --  end of the word and which does not contain any '_' inside

   type String_Entry is record
      String_Chars_Index : Int;
      --  Starting location of characters in the String_Chars table minus one
      --  (i.e. pointer to character just before first character). The reason
      --  for the bias of one is that indexes in all the string values we have
      --  to process are one's origin, so this avoids unnecessary adds and
      --  subtracts of 1.

      String_Len : Short;
      --  Length of this name in characters

      Hash_Link : String_Id;
      --  Link to next entry in names table for same hash code

      Casing_Exception_Kind : Casing_Exception_Kinds;
   end record;

   package String_Entries is new Table.Table
     (Table_Component_Type => String_Entry,
      Table_Index_Type     => String_Id,
      Table_Low_Bound      => First_String_Id,
      Table_Initial        => Alloc.Names_Initial,
      Table_Increment      => Alloc.Names_Increment,
      Table_Name           => "PP_Name_Entries");

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of String_Id;
   --  The hash table is used to locate existing entries in the strings table.
   --  The entries point to the first strings table entry whose hash value
   --  matches the hash code. Then subsequent string table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   -----------------------
   -- Local subprograms --
   -----------------------

   function Hash (Name : String) return Hash_Index_Type;
   --  Compute hash code for its argument

   function Present (Id : String_Id) return Boolean;
   --  Checks if the argument is not equal to No_String

   procedure Add_To_Dictionary
     (Name           : String;
      Exception_Kind : Casing_Exception_Kinds);
   --  If Name does not exist in the dictionary, adds the corresponding
   --  dictionary entry. Otherwise replace the casing defined by the
   --  existing occurrence of this name by the casing given by Name

   function Find_In_Dictionary
     (Name           : String;
      Exception_Kind : Casing_Exception_Kinds)
      return           String_Id;
   --  Tries to find in the dictionary the entry which corresponds to Name
   --  without taking into account the character casing. (Exception_Kind
   --  is used to limit the search by the corresponding kind of dictionary
   --  entries). Return the Id of the corresponding dictionary entry, returns
   --  No_String if the dictionary does not contain such and entry

   function Get_String_From_Dictionary (Id : String_Id) return String;
   --  Returns the string stored in the given dictionary entry. Note, that Id
   --  has to denote the existing entry.

   function Same_Name (Left : String; Right : String) return Boolean;
   --  Checks if Left and Right denote the same name in Ada
   --  (non-case-sensitive) sense

   -----------------------
   -- Add_To_Dictionary --
   -----------------------

   procedure Add_To_Dictionary
     (Name           : String;
      Exception_Kind : Casing_Exception_Kinds)
   is
      Name_Var : constant String (1 .. Name'Length) := Name;
      --  We use it instead of Name to be sure that we have an index range
      --  starting from 1

      Dictionary_Entry    : String_Id;
      Hash_Ind            : Hash_Index_Type;
      New_Id              : String_Id;
      Old_Char_Ind        : Int;
      Traverse_Hash_Chain : Boolean := True;

   begin
      Hash_Ind := Hash (Name_Var);

      if not Present (Hash_Table (Hash_Ind)) then
         Dictionary_Entry      := No_String;
         Traverse_Hash_Chain   := False;
         Hash_Table (Hash_Ind) := String_Entries.Last + 1;
      else
         Dictionary_Entry := Find_In_Dictionary (Name_Var, Exception_Kind);
      end if;

      if not Present (Dictionary_Entry) then
         --  Adding the new dictionary entry

         String_Entries.Append
           (New_Val =>
              (String_Chars_Index    => Name_Chars.Last,
               String_Len            => Name_Var'Length,
               Hash_Link             => No_String,
               Casing_Exception_Kind => Exception_Kind));

         for J in Name_Var'Range loop
            Name_Chars.Increment_Last;
            Name_Chars.Table (Name_Chars.Last) := Name_Var (J);
         end loop;

         if Traverse_Hash_Chain then
            New_Id := Hash_Table (Hash_Ind);

            while Present (String_Entries.Table (New_Id).Hash_Link) loop
               New_Id := String_Entries.Table (New_Id).Hash_Link;
            end loop;

            String_Entries.Table (New_Id).Hash_Link := String_Entries.Last;
         end if;

      elsif Get_String_From_Dictionary (Dictionary_Entry) /= Name_Var then

         --  Correcting the existing dictionary entry

         Old_Char_Ind :=
           String_Entries.Table (Dictionary_Entry).String_Chars_Index;

         for J in Name_Var'Range loop
            Name_Chars.Table (Old_Char_Ind + Int (J)) := Name_Var (J);
         end loop;
      end if;
   end Add_To_Dictionary;

   ---------------------------
   -- Check_With_Dictionary --
   ---------------------------

   procedure Check_With_Dictionary
     (Ada_Name : in out Program_Text;
      Casing   : PP_Casing)
   is
      Name : String := To_String (Ada_Name);

      Name_Last : constant Natural := Name'Last;
      SW_Start  : Integer          := Name'First;
      SW_End    : Integer          := Name_Last;
      --  Indexes of a subword in the Name

      Dictionary_String : String_Id;

      procedure Set_Subword;
      --  Provided that Name has subwords, and that the current settings of
      --  SW_Start and SW_End point to some subword, sets these indexes to
      --  point to the next subword. Set SW_Start and SW_End to 0 if there
      --  is no subwords any more

      --  This procedure does not check if we have one more subword to move
      --  these indexes to.

      function Capitalize_Subword
        (SW     : String;
         Casing : PP_Casing)
         return   String;
      --  Supposing that SW is a (sub)word having no '_' inside, returns
      --  the capitalized version of this subword according to the casing
      --  represented by Casing

      -----------------
      -- Set_Subword --
      -----------------

      procedure Set_Subword is
      begin

         if SW_End = Name_Last then
            --  There is no more subwords
            SW_Start := 0;
            SW_End   := 0;
         else
            SW_Start := SW_End + 2;
            SW_End   := Name_Last;

            for J in SW_Start + 1 .. SW_End loop
               if Name (J) = '_' then
                  SW_End := J - 1;

                  exit;
               end if;
            end loop;

         end if;

      end Set_Subword;

      ------------------------
      -- Capitalize_Subword --
      ------------------------

      function Capitalize_Subword
        (SW     : String;
         Casing : PP_Casing)
         return   String
      is
         Result    : String           := SW;
         First_Idx : constant Natural := Result'First;

      begin
         case Casing is
            when Lower_Case =>
               Result := To_Lower (Result);
            when Upper_Case =>
               Result := To_Upper (Result);
            when Mixed =>
               Result             := To_Lower (Result);
               Result (First_Idx) := To_Upper (Result (First_Idx));

            when As_Declared =>
               --  Nothing to do!
               null;
         end case;

         return Result;
      end Capitalize_Subword;

   begin  --  Check_With_Dictionary
      for J in Name'Range loop
         if Name (J) = '_' then
            SW_End := J - 1;
            exit;
         end if;
      end loop;

      Dictionary_String :=
        Find_In_Dictionary (Name => Name, Exception_Kind => Whole_Word);

      if not Present (Dictionary_String) then
         --  May be we can apply the subword exception to the whole word
         Dictionary_String :=
           Find_In_Dictionary (Name => Name, Exception_Kind => Subword);
      end if;

      if Present (Dictionary_String) then
         Name := Get_String_From_Dictionary (Dictionary_String);
      else

         if SW_End < Name'Last then
            --  That is, the whole word is not in the dictionary and it has at
            --  least two subwords, and Name (SW_Start .. SW) is the first one

            while SW_End /= 0 loop
               Dictionary_String :=
                 Find_In_Dictionary
                   (Name           => Name (SW_Start .. SW_End),
                    Exception_Kind => Subword);

               if Present (Dictionary_String) then
                  Name (SW_Start .. SW_End) :=
                    Get_String_From_Dictionary (Dictionary_String);
               else
                  Name (SW_Start .. SW_End) :=
                    Capitalize_Subword (Name (SW_Start .. SW_End), Casing);
               end if;

               Set_Subword;
            end loop;

         else
            --  The case of a word with no subwords, the word is not in the
            --  dictionary

            Name := Capitalize_Subword (Name, Casing);
         end if;
      end if;

      Ada_Name := To_Wide_String (Name);
   end Check_With_Dictionary;

   ------------------------
   -- Find_In_Dictionary --
   ------------------------

   function Find_In_Dictionary
     (Name           : String;
      Exception_Kind : Casing_Exception_Kinds)
      return           String_Id
   is
      Result  : String_Id := No_String;
      Next_Id : String_Id;

   begin
      Next_Id := Hash_Table (Hash (Name));

      while Present (Next_Id) loop

         if String_Entries.Table (Next_Id).Casing_Exception_Kind =
           Exception_Kind
           and then Same_Name (Name, Get_String_From_Dictionary (Next_Id))
         then
            Result := Next_Id;
            exit;
         end if;

         Next_Id := String_Entries.Table (Next_Id).Hash_Link;
      end loop;

      return Result;
   end Find_In_Dictionary;

   --------------------------------
   -- Get_String_From_Dictionary --
   --------------------------------

   function Get_String_From_Dictionary (Id : String_Id) return String is
      Res_Len : constant Integer :=
        Integer (String_Entries.Table (Id).String_Len);

      Chars_Start : constant Int :=
        String_Entries.Table (Id).String_Chars_Index;

      Result : String (1 .. Res_Len);

   begin
      for J in 1 .. Res_Len loop
         Result (J) := Name_Chars.Table (Chars_Start + Int (J));
      end loop;

      return Result;
   end Get_String_From_Dictionary;

   ----------
   -- Hash --
   ----------

   function Hash (Name : String) return Hash_Index_Type is
      subtype Int_1_12 is Int range 1 .. 12;
      --  Used to avoid when others on case jump below

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

      --  We take one-to-one the code of the Hash function from Namet
      --  (namet.adb, rev. 1.90). Namet.Hash function works on the name buffer
      --  defined in Namet. We simulate this buffer by defining the following
      --  variables (note converting the argument string to lower case before
      --  computing the hash value):

      Name_Buffer : constant String  := To_Lower (Name);
      Name_Len    : constant Natural := Name_Buffer'Last;

   begin
      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return
           ((
             ((
               ((
                 ((
                   ((
                     ((Character'Pos (Name_Buffer (01))) * 2 +
                      Character'Pos (Name_Buffer (Even_Name_Len - 10))) *
                     2 +
                     Character'Pos (Name_Buffer (03))) *
                    2 +
                    Character'Pos (Name_Buffer (Even_Name_Len - 08))) *
                   2 +
                   Character'Pos (Name_Buffer (05))) *
                  2 +
                  Character'Pos (Name_Buffer (Even_Name_Len - 06))) *
                 2 +
                 Character'Pos (Name_Buffer (07))) *
                2 +
                Character'Pos (Name_Buffer (Even_Name_Len - 04))) *
               2 +
               Character'Pos (Name_Buffer (09))) *
              2 +
              Character'Pos (Name_Buffer (Even_Name_Len - 02))) *
             2 +
             Character'Pos (Name_Buffer (11))) *
            2 +
            Character'Pos (Name_Buffer (Even_Name_Len))) mod
           Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_1_12 (Name_Len) is
         when 1 =>
            return Character'Pos (Name_Buffer (1));

         when 2 =>
            return
              ((Character'Pos (Name_Buffer (1))) * 64 +
               Character'Pos (Name_Buffer (2))) mod
              Hash_Num;

         when 3 =>
            return
              (((Character'Pos (Name_Buffer (1))) * 16 +
                Character'Pos (Name_Buffer (3))) *
               16 +
               Character'Pos (Name_Buffer (2))) mod
              Hash_Num;

         when 4 =>
            return
              ((
                ((Character'Pos (Name_Buffer (1))) * 8 +
                 Character'Pos (Name_Buffer (2))) *
                8 +
                Character'Pos (Name_Buffer (3))) *
               8 +
               Character'Pos (Name_Buffer (4))) mod
              Hash_Num;

         when 5 =>
            return
              ((
                (((Character'Pos (Name_Buffer (4))) * 8 +
                  Character'Pos (Name_Buffer (1))) *
                 4 +
                 Character'Pos (Name_Buffer (3))) *
                4 +
                Character'Pos (Name_Buffer (5))) *
               8 +
               Character'Pos (Name_Buffer (2))) mod
              Hash_Num;

         when 6 =>
            return
              ((
                ((
                  ((Character'Pos (Name_Buffer (5))) * 4 +
                   Character'Pos (Name_Buffer (1))) *
                  4 +
                  Character'Pos (Name_Buffer (4))) *
                 4 +
                 Character'Pos (Name_Buffer (2))) *
                4 +
                Character'Pos (Name_Buffer (6))) *
               4 +
               Character'Pos (Name_Buffer (3))) mod
              Hash_Num;

         when 7 =>
            return
              ((
                ((
                  (((Character'Pos (Name_Buffer (4))) * 4 +
                    Character'Pos (Name_Buffer (3))) *
                   4 +
                   Character'Pos (Name_Buffer (1))) *
                  4 +
                  Character'Pos (Name_Buffer (2))) *
                 2 +
                 Character'Pos (Name_Buffer (5))) *
                2 +
                Character'Pos (Name_Buffer (7))) *
               2 +
               Character'Pos (Name_Buffer (6))) mod
              Hash_Num;

         when 8 =>
            return
              ((
                ((
                  ((
                    ((Character'Pos (Name_Buffer (2))) * 4 +
                     Character'Pos (Name_Buffer (1))) *
                    4 +
                    Character'Pos (Name_Buffer (3))) *
                   2 +
                   Character'Pos (Name_Buffer (5))) *
                  2 +
                  Character'Pos (Name_Buffer (7))) *
                 2 +
                 Character'Pos (Name_Buffer (6))) *
                2 +
                Character'Pos (Name_Buffer (4))) *
               2 +
               Character'Pos (Name_Buffer (8))) mod
              Hash_Num;

         when 9 =>
            return
              ((
                ((
                  ((
                    (((Character'Pos (Name_Buffer (2))) * 4 +
                      Character'Pos (Name_Buffer (1))) *
                     4 +
                     Character'Pos (Name_Buffer (3))) *
                    4 +
                    Character'Pos (Name_Buffer (4))) *
                   2 +
                   Character'Pos (Name_Buffer (8))) *
                  2 +
                  Character'Pos (Name_Buffer (7))) *
                 2 +
                 Character'Pos (Name_Buffer (5))) *
                2 +
                Character'Pos (Name_Buffer (6))) *
               2 +
               Character'Pos (Name_Buffer (9))) mod
              Hash_Num;

         when 10 =>
            return
              ((
                ((
                  ((
                    ((
                      ((Character'Pos (Name_Buffer (01))) * 2 +
                       Character'Pos (Name_Buffer (02))) *
                      2 +
                      Character'Pos (Name_Buffer (08))) *
                     2 +
                     Character'Pos (Name_Buffer (03))) *
                    2 +
                    Character'Pos (Name_Buffer (04))) *
                   2 +
                   Character'Pos (Name_Buffer (09))) *
                  2 +
                  Character'Pos (Name_Buffer (06))) *
                 2 +
                 Character'Pos (Name_Buffer (05))) *
                2 +
                Character'Pos (Name_Buffer (07))) *
               2 +
               Character'Pos (Name_Buffer (10))) mod
              Hash_Num;

         when 11 =>
            return
              ((
                ((
                  ((
                    ((
                      (((Character'Pos (Name_Buffer (05))) * 2 +
                        Character'Pos (Name_Buffer (01))) *
                       2 +
                       Character'Pos (Name_Buffer (06))) *
                      2 +
                      Character'Pos (Name_Buffer (09))) *
                     2 +
                     Character'Pos (Name_Buffer (07))) *
                    2 +
                    Character'Pos (Name_Buffer (03))) *
                   2 +
                   Character'Pos (Name_Buffer (08))) *
                  2 +
                  Character'Pos (Name_Buffer (02))) *
                 2 +
                 Character'Pos (Name_Buffer (10))) *
                2 +
                Character'Pos (Name_Buffer (04))) *
               2 +
               Character'Pos (Name_Buffer (11))) mod
              Hash_Num;

         when 12 =>
            return
              ((
                ((
                  ((
                    ((
                      ((
                        ((Character'Pos (Name_Buffer (03))) * 2 +
                         Character'Pos (Name_Buffer (02))) *
                        2 +
                        Character'Pos (Name_Buffer (05))) *
                       2 +
                       Character'Pos (Name_Buffer (01))) *
                      2 +
                      Character'Pos (Name_Buffer (06))) *
                     2 +
                     Character'Pos (Name_Buffer (04))) *
                    2 +
                    Character'Pos (Name_Buffer (08))) *
                   2 +
                   Character'Pos (Name_Buffer (11))) *
                  2 +
                  Character'Pos (Name_Buffer (07))) *
                 2 +
                 Character'Pos (Name_Buffer (09))) *
                2 +
                Character'Pos (Name_Buffer (10))) *
               2 +
               Character'Pos (Name_Buffer (12))) mod
              Hash_Num;
      end case;
   end Hash;

   -------------
   -- Present --
   -------------

   function Present (Id : String_Id) return Boolean is
   begin
      return Id /= No_String;
   end Present;

   --  Checks if the argument is not equal to No_String
   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (Left : String; Right : String) return Boolean is
      Result : Boolean          := True;
      Move   : constant Integer := Right'First - Left'First;

   begin
      if Left'Length /= Right'Length then
         Result := False;
      else
         for J in Left'Range loop
            if To_Lower (Left (J)) /= To_Lower (Right (J + Move)) then
               Result := False;
               exit;
            end if;
         end loop;
      end if;

      return Result;
   end Same_Name;

   ---------------------
   -- Scan_Dictionary --
   ---------------------

   procedure Scan_Dictionary (Dictionary_Name : String) is
      String_Buffer_Max_Len : constant Natural := 1024;
      --  Should be enough, I hope...

      String_Buffer : String (1 .. String_Buffer_Max_Len);

      Len : Natural range 0 .. String_Buffer_Max_Len := 0;
      --  The length of the dictionary file line which is being processed

      Line_Num : Natural := 0;
      --  The number of the currently processed line

      Dictionary_File : File_Type;

      procedure Process_Dictionary_File_Line;
      --  Reads the next line from the dictionary file, parses it, and
      --  if founds the new definition of the casing exception, puts the
      --  corresponding word in the exception table

      ----------------------------------
      -- Process_Dictionary_File_Line --
      ----------------------------------

      procedure Process_Dictionary_File_Line is
         Start_Word : Natural := 0;
         End_Word   : Natural := 0;

         Exc_Kind : Casing_Exception_Kinds;

         function Skip_White_Spaces (Idx : Natural) return Natural;
         --  Starting from Idx (which is treated as an index in String_Buffer
         --  bounded by the current value of Len), computes the index of the
         --  first non-blank character. If there is no non-blank character on
         --  the right from Idx, or if the actual is greater than Len returns
         --  zero.

         function Skip_Non_Space_Chars (Idx : Natural) return Natural;
         --  Starting from Idx (which is treated as an index in String_Buffer
         --  bounded by the current value of Len, Idx is supposed to point to
         --  non-blank character), return the index of the character preceding
         --  the first white space character (or Ada comment beginning) being
         --  on the right of Idx (returns Len if there is no such white space)

         function Get_Exception_Kind return Casing_Exception_Kinds;
         --  Checks if String_Buffer (Start_Word .. End_Word) has a syntax of
         --  a casing exception and return the corresponding exception kind.
         --  Returns Not_A_Casing_Exception if this word can not be interpreted
         --  as a casing exception As a side effect, this function may correct
         --  the values of Start_Word and End_Word to skip '*' in case of a
         --  subword

         function Is_White_Space (Ch : Character) return Boolean;
         --  Checks if Ch is a white space

         --------------------
         -- Is_White_Space --
         --------------------

         function Is_White_Space (Ch : Character) return Boolean is
         begin
            return False or else Ch = ' ' or else Ch = ASCII.HT;
         end Is_White_Space;

         -----------------------
         -- Skip_White_Spaces --
         -----------------------

         function Skip_White_Spaces (Idx : Natural) return Natural is
            Result : Natural := Idx;
         begin
            while Is_White_Space (String_Buffer (Result)) and then Result < Len
            loop
               Result := Result + 1;
            end loop;

            if Result > Len
              or else
              (Result = Len and then Is_White_Space (String_Buffer (Result)))
            then
               Result := 0;
            end if;

            return Result;
         end Skip_White_Spaces;

         --------------------------
         -- Skip_Non_Space_Chars --
         --------------------------

         function Skip_Non_Space_Chars (Idx : Natural) return Natural is
            Result : Natural := Idx;
         begin
            while Result < Len
              and then not
              (Is_White_Space (String_Buffer (Result))
               or else
               (String_Buffer (Result) = '-'
                and then String_Buffer (Result + 1) = '-'))
            loop
               Result := Result + 1;
            end loop;

            if Is_White_Space (String_Buffer (Result))
              or else String_Buffer (Result) = '-'
            then
               Result := Result - 1;
            end if;

            return Result;
         end Skip_Non_Space_Chars;

         ------------------------
         -- Get_Exception_Kind --
         ------------------------

         function Get_Exception_Kind return Casing_Exception_Kinds is
            Result : Casing_Exception_Kinds;

            Prev_Char_Is_Underline : Boolean := False;
         begin

            if String_Buffer (Start_Word) = '*'
              and then String_Buffer (End_Word) = '*'
            then
               Result     := Subword;
               Start_Word := Start_Word + 1;
               End_Word   := End_Word - 1;
            else
               Result := Whole_Word;
            end if;

            --  And now we have to check that String_Buffer (First_Idx ..
            --  Last_Idx) has a syntax of an identifier

            if Start_Word > End_Word
              or else String_Buffer (Start_Word) = '_'
              or else String_Buffer (End_Word) = '_'
            then
               Result := Not_A_Casing_Exception;
            else

               for J in Start_Word .. End_Word loop

                  if Is_Alphanumeric (String_Buffer (J)) then
                     Prev_Char_Is_Underline := False;
                  elsif String_Buffer (J) = '_' then

                     if not Prev_Char_Is_Underline then

                        Prev_Char_Is_Underline := True;

                        if Result = Subword then
                           Result := Not_A_Casing_Exception;
                           exit;
                        end if;

                     else
                        Result := Not_A_Casing_Exception;
                        exit;
                     end if;

                  else
                     Result := Not_A_Casing_Exception;
                     exit;
                  end if;
               end loop;
            end if;

            return Result;
         end Get_Exception_Kind;

      begin  --  Process_Dictionary_File_Line
         Get_Line (Dictionary_File, String_Buffer, Len);

         if Len = 0 then
            --  This is an empty line
            return;
         end if;

         Start_Word := Skip_White_Spaces (1);

         if Start_Word = 0
           or else
           (Start_Word < Len
            and then String_Buffer (Start_Word) = '-'
            and then String_Buffer (Start_Word + 1) = '-')
         then
            --  blank or comment line
            return;
         end if;

         End_Word := Skip_Non_Space_Chars (Start_Word);

         Exc_Kind := Get_Exception_Kind;

         if Exc_Kind = Not_A_Casing_Exception then
            Ada.Text_IO.Put_Line
              (Standard_Error,
               Dictionary_Name &
               ':' &
               Image (Line_Num) &
               ':' &
               Image (Start_Word) &
               ": wrong syntax of a casing exception, line ignored");

         else
            Add_To_Dictionary
              (String_Buffer (Start_Word .. End_Word),
               Exc_Kind);

            --  We have to check if we have something else in the dictionary
            --  file line. The only possible things are blank characters and
            --  comments

            if End_Word < Len and then String_Buffer (End_Word + 1) = '*' then
               --  Taking into account the side effect of Get_Exception_Kind
               End_Word := End_Word + 1;
            end if;

            if End_Word < Len then
               --  We have something else in this line

               Start_Word := Skip_White_Spaces (End_Word + 1);

               if not
                 (Start_Word = 0
                  or else
                  (Start_Word < Len
                   and then String_Buffer (Start_Word) = '-'
                   and then String_Buffer (Start_Word + 1) = '-'))
               then
                  Ada.Text_IO.Put_Line
                    (Standard_Error,
                     Dictionary_Name &
                     ':' &
                     Image (Line_Num) &
                     ':' &
                     Image (Start_Word) &
                     ": only one casing exception per line is allowed");
                  Ada.Text_IO.Put_Line
                    (Standard_Error,
                     Dictionary_Name &
                     ':' &
                     Image (Line_Num) &
                     ':' &
                     Image (Start_Word) &
                     ": end of line ignored");
               end if;
            end if;
         end if;
      end Process_Dictionary_File_Line;

   begin  --  Scan_Dictionary

      --  First trying to open the dictionary file: ???It would be cleaner to
      --  keep the file opening and error message handling in gnatpp.

      begin
         Open
           (File => Dictionary_File,
            Mode => In_File,
            Name => Dictionary_Name);
      exception
         when Name_Error =>
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "gnatpp: can not find dictionary file " & Dictionary_Name);
            return;

         when Status_Error =>
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "gnatpp: can not open dictionary file " & Dictionary_Name);
            Ada.Text_IO.Put_Line
              (Standard_Error,
               "        the file may be used by another process");
            return;
      end;

      while not End_Of_File (Dictionary_File) loop
         Line_Num := Line_Num + 1;
         Process_Dictionary_File_Line;
      end loop;

      if Is_Open (Dictionary_File) then
         Close (Dictionary_File);
      end if;

   end Scan_Dictionary;

begin
   --  We put the initialization of the Dictionary tables here, because it is
   --  not supposed that a dictionary is the same for the whole gnatpp run.

   Name_Chars.Init;
   String_Entries.Init;

   for J in Hash_Index_Type loop
      Hash_Table (J) := No_String;
   end loop;
end Ada_Trees.Formatting.Dictionaries;
