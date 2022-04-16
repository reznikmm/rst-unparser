--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

pragma Warnings (Off, "unrecognized pragma");
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (Off, "unrecognized pragma");

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Regexps;

package body RST_Unparsers is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function Count_Enclosing
     (Self : RST_Unparser'Class;
      Kind : Unparser_State_Kind) return Natural;

   function Get_Literal_Role
     (Attributes : XML.SAX.Attributes.SAX_Attributes)
       return League.Strings.Universal_String;

   procedure Emit_Characters (Self : in out RST_Unparser'Class);
   procedure Emit_Indent
     (Self       : in out RST_Unparser'Class;
      First_Line : Boolean);

   procedure Append_URI
     (Self  : in out RST_Unparser'Class;
      URI   : League.Strings.Universal_String;
      Title : League.Strings.Universal_String);

   procedure Print_Table_Row_Separator (Self : in out RST_Unparser'Class);

   function Trim
     (List : League.String_Vectors.Universal_String_Vector)
       return League.String_Vectors.Universal_String_Vector;

   function Image (Value : Positive) return League.Strings.Universal_String;

   Skip_Space_Pattern : constant League.Regexps.Regexp_Pattern :=
     League.Regexps.Compile (+"^\ *([^\ ].*)$");

   ----------------
   -- Append_URI --
   ----------------

   procedure Append_URI
     (Self  : in out RST_Unparser'Class;
      URI   : League.Strings.Universal_String;
      Title : League.Strings.Universal_String) is
   begin
      Self.URI_Map.Insert (Title, URI);
   end Append_URI;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out RST_Unparser;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Characters.Append (Text);
   end Characters;

   ---------------------
   -- Count_Enclosing --
   ---------------------

   function Count_Enclosing
     (Self : RST_Unparser'Class;
      Kind : Unparser_State_Kind) return Natural
   is
      Result : Natural := 0;
   begin
      for Item of Self.Stack loop
         if Item.Kind = Kind then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Count_Enclosing;

   ---------------------
   -- Emit_Characters --
   ---------------------

   procedure Emit_Characters (Self : in out RST_Unparser'Class) is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Self.Characters.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
   begin
      if Self.Characters.Is_Empty then
         return;
      end if;

      for Top of reverse Self.Stack loop
         case Top.Kind is
            when Paragraph =>
               for J in 1 .. List.Length loop
                  if J > 1 then
                     Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
                     Self.Emit_Indent (False);

                     declare
                        Match : constant League.Regexps.Regexp_Match :=
                          Skip_Space_Pattern.Find_Match (List (J));
                     begin
                        if Match.Is_Matched then  --  Something but spaces
                           Self.Current.Put (Match.Capture (1));
                        end if;
                     end;
                  else
                     Self.Current.Put (List (J));
                  end if;
               end loop;

               return;

            when Literal_Block =>
               for J in 1 .. List.Length loop
                  Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);

                  if not List (J).Is_Empty then
                     Self.Emit_Indent (J = 1);
                  end if;

                  Self.Current.Put (List (J));
               end loop;

               return;

            when Literal =>
               Self.Current.Put (Self.Characters);
               return;

            when Ignore =>
               null;
            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Emit_Characters;

   -----------------
   -- Emit_Indent --
   -----------------

   procedure Emit_Indent
     (Self       : in out RST_Unparser'Class;
      First_Line : Boolean)
   is
      In_List : constant Natural := Self.Count_Enclosing (List_Item);
      Indent  : constant Natural :=
        Self.Count_Enclosing (Admonition) * 4
        + Self.Count_Enclosing (Hint) * 4
        + Self.Count_Enclosing (Note) * 4
        + Self.Count_Enclosing (Attention) * 4
        + Self.Count_Enclosing (Literal_Block) * 4
        + (if In_List = 0 or First_Line then 0 else 3 * In_List);
   begin
      for J in 1 .. Indent loop
         Self.Current.Put (' ');
      end loop;
   end Emit_Indent;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self           : in out RST_Unparser;
      Namespace_URI  :        League.Strings.Universal_String;
      Local_Name     :        League.Strings.Universal_String;
      Qualified_Name :        League.Strings.Universal_String;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Success);
      Top : constant Unparser_State := Self.Stack.Last_Element;
   begin
      case Top.Kind is
         when Document | Ignore | Section | Admonition | Hint | Note
            | Attention =>
            null;
         when Title =>
            Self.Current.Put (Self.Characters);
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);

            case Self.Stack (Self.Stack.Last_Index - 1).Kind is
               when Admonition =>
                  Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
               when Section =>
                  declare
                     Depth : constant Positive :=
                       Self.Count_Enclosing (Section);

                     Separator : constant Wide_Wide_Character :=
                       (case Depth is
                           when 1 => '=',
                           when 2 => '-',
                           when 3 => '~',
                           when others => '?');
                  begin
                     for J in 1 .. Self.Characters.Length loop
                        Self.Current.Put (Separator);
                     end loop;

                     Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
                     Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
                  end;
               when others =>
                  raise Program_Error;
            end case;
         when Paragraph =>
            Self.Emit_Characters;
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         when Literal_Block =>
            Self.Emit_Characters;
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         when Inline =>
            Self.Current.Put (Self.Characters);
         when Literal =>
            Self.Current.Put (Self.Characters);
            Self.Current.Put ("`");

            if Top.Empty_Role then
               Self.Current.Put ("`");
            end if;
         when Emphasis =>
            Self.Current.Put (Self.Characters);
            Self.Current.Put ("*");
         when Strong =>
            Self.Current.Put (Self.Characters);
            Self.Current.Put ("**");
         when Reference =>
            Self.Current.Put (Self.Characters);
            Self.Current.Put (" <");
            if Top.Internal then
               pragma Assert
                 (Self.URI_Map.Contains (Top.URI),
                  "missing " & Top.URI.To_UTF_8_String);
               Self.Current.Put (Self.URI_Map (Top.URI));
               Self.Current.Put (">`");
            else
               Self.Current.Put (Top.URI);
               Self.Current.Put (">`_");
            end if;
         when Bullet_List | Enumerated_List | List_Item =>
            null;
         when Table =>
            --  Free (Self.Table.Output);
            Print_Table_Row_Separator (Self);
            Self.Current := Self.Table.Parent;
            Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
            Self.Table := Top.Table;
         when Row =>
            declare
               Output : access
                 XML.SAX.Output_Destinations.SAX_Output_Destination'Class
                   renames Self.Table.Parent;

               Line  : Positive := 1;
               Lines : array (1 .. Self.Table.Columns) of
                 League.String_Vectors.Universal_String_Vector;
            begin
               for J in 1 .. Self.Table.Text.Length loop
                  Lines (J) := Trim
                    (Self.Table.Text (J).Split
                      (Ada.Characters.Wide_Wide_Latin_1.LF));
               end loop;

               Print_Table_Row_Separator (Self);

               while (for some Vector of Lines => Line <= Vector.Length) loop
                  for Column in 1 .. Self.Table.Columns loop
                     Output.Put ("| ");

                     if Line <= Lines (Column).Length then
                        Output.Put (Lines (Column) (Line));

                        for J in Lines (Column) (Line).Length
                          .. Self.Table.Width (Column) - 2
                        loop
                           Output.Put (' ');
                        end loop;
                     else
                        for J in 0 .. Self.Table.Width (Column) - 2 loop
                           Output.Put (' ');
                        end loop;
                     end if;
                  end loop;

                  Output.Put ('|');
                  Output.Put (Ada.Characters.Wide_Wide_Latin_1.LF);

                  Line := Line + 1;
               end loop;
            end;
         when Column =>
            Self.Table.Text.Append (Self.Table.Output.Get_Text);
            Self.Table.Column := Self.Table.Column + 1;
      end case;

      Self.Characters.Clear;
      Self.Stack.Delete_Last;
   end End_Element;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : RST_Unparser) return League.Strings.Universal_String
   is
   begin
      return +"Unimplemented function Error_String";
   end Error_String;

   ----------------------
   -- Get_Literal_Role --
   ----------------------

   function Get_Literal_Role
     (Attributes : XML.SAX.Attributes.SAX_Attributes)
       return League.Strings.Universal_String
   is
      Classes : constant League.Strings.Universal_String := Attributes.Value
        (League.Strings.Empty_Universal_String, +"classes");
      List    : constant League.String_Vectors.Universal_String_Vector :=
        Classes.Split (' ');
   begin
      if List.Length > 0 then
         return List (List.Length);
      else
         return League.Strings.Empty_Universal_String;
      end if;
   end Get_Literal_Role;

   -----------
   -- Image --
   -----------

   function Image (Value : Positive) return League.Strings.Universal_String is
      Image : constant Wide_Wide_String := Value'Wide_Wide_Image;
   begin
      return +Image (2 .. Image'Last);
   end Image;

   -------------------------------
   -- Print_Table_Row_Separator --
   -------------------------------

   procedure Print_Table_Row_Separator (Self : in out RST_Unparser'Class) is
      Output : access XML.SAX.Output_Destinations.SAX_Output_Destination'Class
        renames Self.Table.Parent;
   begin
      for Column in 1 .. Self.Table.Columns loop
         Output.Put ('+');
         for J in 1 .. Self.Table.Width (Column) loop
            Output.Put ('-');
         end loop;
      end loop;

      Output.Put ('+');
      Output.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
   end Print_Table_Row_Separator;

   ------------------
   -- Read_URI_Map --
   ------------------

   procedure Read_URI_Map
     (Handler : in out RST_Unparsers.RST_Unparser;
      File    : League.Strings.Universal_String)
   is
      Input : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (Input, Ada.Wide_Wide_Text_IO.In_File, File.To_UTF_8_String);

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Title : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (Input);

            URI   : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (Input);
         begin
            Handler.Append_URI
              (League.Strings.To_Universal_String (URI),
               League.Strings.To_Universal_String (Title));
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read_URI_Map;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
    (Self    : in out RST_Unparser;
     Locator : XML.SAX.Locators.SAX_Locator) is
   begin
      Self.Locator := Locator;
   end Set_Document_Locator;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
    (Self    : in out RST_Unparser;
     Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Current := Self.Output;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self           : in out RST_Unparser;
      Namespace_URI  : League.Strings.Universal_String;
      Local_Name     : League.Strings.Universal_String;
      Qualified_Name : League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Success);
      Name : constant Wide_Wide_String := Local_Name.To_Wide_Wide_String;
   begin
      if Name = "document" then
         Self.Stack.Append (Unparser_State'(Kind => Document));
      elsif Name = "section" then
         Self.Stack.Append (Unparser_State'(Kind => Section));
      elsif Name = "admonition" then
         Self.Current.Put (".. admonition:: ");
         Self.Stack.Append (Unparser_State'(Kind => Admonition));
      elsif Name = "hint" then
         Self.Current.Put (".. hint::");
         Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         Self.Stack.Append (Unparser_State'(Kind => Hint));
      elsif Name = "note" then
         Self.Current.Put (".. note::");
         Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         Self.Stack.Append (Unparser_State'(Kind => Note));
      elsif Name = "attention" then
         Self.Current.Put (".. attention::");
         Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         Self.Stack.Append (Unparser_State'(Kind => Attention));
      elsif Name = "title" then
         Self.Stack.Append (Unparser_State'(Kind => Title));
      elsif Name = "paragraph" then
         Self.Emit_Indent (True);
         Self.Stack.Append (Unparser_State'(Kind => Paragraph));
      elsif Name = "literal_block" then
         Self.Current.Put (".. code-block:: ada");
         Self.Current.Put (Ada.Characters.Wide_Wide_Latin_1.LF);
         Self.Stack.Append (Unparser_State'(Kind => Literal_Block));
      elsif Name = "bullet_list" then
         Self.Stack.Append (Unparser_State'(Kind => Bullet_List));
      elsif Name = "enumerated_list" then
         Self.Stack.Append (Unparser_State'(Kind => Enumerated_List, Item => 1));
      elsif Name = "list_item" then
         for J in 1 .. Self.Count_Enclosing (List_Item) * 3 loop
            Self.Current.Put (' ');
         end loop;

         if Self.Stack.Last_Element.Kind = Enumerated_List then
            Self.Current.Put (Image (Self.Stack (Self.Stack.Last_Index).Item));
            Self.Current.Put (". ");
            Self.Stack (Self.Stack.Last_Index).Item := @ + 1;
         else
            Self.Current.Put ("-  ");
         end if;
         Self.Stack.Append (Unparser_State'(Kind => List_Item));
      --  Inline elements
      elsif Name in "inline"
        | "literal_strong"
        | "emphasis"
        | "strong"
        | "superscript"
        | "reference"
        | "title_reference"
        | "literal"
      then
         Self.Emit_Characters;

         if Name in "literal" | "literal_strong" then
            declare
               Role : constant League.Strings.Universal_String :=
                 Get_Literal_Role (Attributes);
            begin
               if not Role.Is_Empty then
                  Self.Current.Put (":");
                  Self.Current.Put (Role);
                  Self.Current.Put (":");
               else
                  Self.Current.Put ("`");
               end if;

               Self.Current.Put ("`");
               Self.Stack.Append (Unparser_State'(Literal, Role.Is_Empty));
            end;
         elsif Name in "inline" then
            Self.Stack.Append (Unparser_State'(Kind => Inline));
         elsif Name in "superscript" then
            Self.Current.Put (":sup:`");
            Self.Stack.Append (Unparser_State'(Literal, False));
         elsif Name in "emphasis" then
            Self.Current.Put ("*");
            Self.Stack.Append (Unparser_State'(Kind => Emphasis));
         elsif Name in "strong" then
            Self.Current.Put ("**");
            Self.Stack.Append (Unparser_State'(Kind => Strong));
         elsif Name in "reference" then
            declare
               Internal : constant Boolean :=
                 Attributes.Value (+"internal") = +"True";
            begin
               if Internal then
                  Self.Current.Put (":ref:");
               end if;

               Self.Current.Put ("`");

               Self.Stack.Append
                 (Unparser_State'
                    (Reference,
                     Attributes.Value (+"refuri"),
                     Internal));
            end;
         elsif Name = "title_reference" then
            Self.Current.Put ("`");
            Self.Stack.Append (Unparser_State'(Literal, False));
         else
            Self.Stack.Append (Unparser_State'(Kind => Inline));
         end if;
      elsif Name = "table" then
         Self.Stack.Append (Unparser_State'(Table, Self.Table));
      elsif Name = "tgroup" then
         declare
            Cols : constant Positive := Positive'Wide_Wide_Value
              (Attributes.Value (+"cols").To_Wide_Wide_String);
            Output : constant String_Output_Destination_Access :=
              new XML.SAX.String_Output_Destinations.String_Output_Destination;
         begin
            Self.Table := (Cols, Self.Current, Output, 1, others => <>);
            Self.Current := Output.all'Access;
            Self.Stack.Append (Unparser_State'(Kind => Ignore));
         end;
      elsif Name = "colspec" then
         declare
            Colwidth : constant Positive := Positive'Wide_Wide_Value
              (Attributes.Value (+"colwidth").To_Wide_Wide_String);
         begin
            Self.Table.Width (Self.Table.Column) := Colwidth;
            Self.Table.Column := Self.Table.Column + 1;
            Self.Stack.Append (Unparser_State'(Kind => Ignore));
         end;
      elsif Name = "row" then
         Self.Table.Column := 1;
         Self.Table.Text.Clear;
         Self.Stack.Append (Unparser_State'(Kind => Row));
      elsif Name = "entry" then
         Self.Stack.Append (Unparser_State'(Kind => Column));
         Self.Table.Output.Clear;
      elsif Name in "comment" | "substitution_definition" | "target"
        | "tbody" | "thead" | "block_quote"
      then
         Self.Stack.Append (Unparser_State'(Kind => Ignore));
      else
         raise Program_Error with Local_Name.To_UTF_8_String;
      end if;

      Self.Characters.Clear;
   end Start_Element;

   ----------
   -- Trim --
   ----------

   function Trim
     (List : League.String_Vectors.Universal_String_Vector)
       return League.String_Vectors.Universal_String_Vector is
   begin
      for J in reverse 1 .. List.Length loop
         if not List (J).Is_Empty then
            return List.Slice (1, J);
         end if;
      end loop;

      return League.String_Vectors.Empty_Universal_String_Vector;
   end Trim;

end RST_Unparsers;
