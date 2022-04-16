--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Strings;
with League.String_Vectors;

with XML.SAX.Constants;
with XML.SAX.Content_Handlers;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;
with XML.SAX.String_Output_Destinations;
with Events_Printers;
with RST_Unparsers;

procedure Replace is

   use type Ada.Containers.Count_Type;
   use all type Events_Printers.DOM_Element_Kind;

   function To_Vector
     (Text : League.Strings.Universal_String)
      return League.String_Vectors.Universal_String_Vector;

   procedure Append
     (Lines  : in out League.String_Vectors.Universal_String_Vector;
      Text   : League.Strings.Universal_String;
      Indent : in out Natural);

   procedure Read_DOM
     (File   : League.Strings.Universal_String;
      Result : out Events_Printers.DOM_Trees.Tree);

   procedure Traverse
     (Tree    : Events_Printers.DOM_Trees.Tree;
      Cursor  : Events_Printers.DOM_Trees.Cursor;
      Handler : in out XML.SAX.Content_Handlers.SAX_Content_Handler'Class);

   procedure Replace_Section
     (RST     : in out Events_Printers.DOM_Trees.Tree;
      Section : Events_Printers.DOM_Trees.Cursor;
      Doc     : Events_Printers.DOM_Trees.Tree;
      First   : in out Events_Printers.DOM_Trees.Cursor);

   procedure Replace_Paragraph
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor);

   procedure Replace_Admonition
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor);

   procedure Replace_Bullet_List
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor);

   procedure Skip_Literal_Block
     (Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor);

   function Get_Title
     (Doc  : Events_Printers.DOM_Trees.Tree;
      Next : in out Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String;
   --  Find title in Doc starting from Next. Move Next forward.

   function Get_Paragraph
     (Doc  : Events_Printers.DOM_Trees.Tree;
      Next : in out Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String;
   --  Find paragraph in Doc starting from Next. Move Next forward.

   function Get_Text
     (XML  : Events_Printers.DOM_Trees.Tree;
      Node : Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String;

   procedure Replace_Character
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Text   : League.String_Vectors.Universal_String_Vector);

   function Find_First_Text
     (Doc : Events_Printers.DOM_Trees.Tree)
      return Events_Printers.DOM_Trees.Cursor;

   function Find_Section
     (RST : Events_Printers.DOM_Trees.Tree)
      return Events_Printers.DOM_Trees.Cursor;

   function All_Empty (Lines : League.String_Vectors.Universal_String_Vector)
     return Boolean;
   pragma Unreferenced (All_Empty);

   LF : constant Wide_Wide_Character := Ada.Characters.Wide_Wide_Latin_1.LF;

   ---------------
   -- All_Empty --
   ---------------

   function All_Empty (Lines : League.String_Vectors.Universal_String_Vector)
     return Boolean is
   begin
      for J in 1 .. Lines.Length loop
         if (for some X of Lines (J).To_Wide_Wide_String => X /= ' ') then
            return False;
         end if;
      end loop;

      return True;
   end All_Empty;

   ------------
   -- Append --
   ------------

   procedure Append
     (Lines  : in out League.String_Vectors.Universal_String_Vector;
      Text   : League.Strings.Universal_String;
      Indent : in out Natural)
   is
      Skip  : Natural := Indent;
      Last  : League.Strings.Universal_String;
      First : Boolean := True;
   begin
      if Text.Is_Empty then
         return;
      elsif Lines.Is_Empty then
         Lines.Append (League.Strings.Empty_Universal_String);
      end if;

      Last := Lines (Lines.Length);

      declare
         List : constant League.String_Vectors.Universal_String_Vector :=
           Text.Split (' ');
      begin
         for K in 1 .. List.Length loop
            if Last.Length + 1 + List (K).Length + Skip > 70 then
               Lines.Replace (Lines.Length, Last);
               Lines.Append (League.Strings.Empty_Universal_String);
               Last := List (K);
               Skip := 0;
            else
               if First then
                  First := False;
               else
                  Last.Append (' ');
               end if;

               Last.Append (List (K));
            end if;
         end loop;

         if Skip = 0 then
            Indent := Last.Length;
         else
            Indent := Indent + Last.Length;
         end if;
      end;

      Lines.Replace (Lines.Length, Last);
   end Append;

   ---------------------
   -- Find_First_Text --
   ---------------------

   function Find_First_Text
     (Doc : Events_Printers.DOM_Trees.Tree)
      return Events_Printers.DOM_Trees.Cursor
   is

      Result : Events_Printers.DOM_Trees.Cursor :=  --  office:document
        Events_Printers.DOM_Trees.First_Child
          (Events_Printers.DOM_Trees.First_Child (Doc.Root));

      function Item return Events_Printers.DOM_Element is
        (Events_Printers.DOM_Trees.Element (Result));

   begin
      while Item.Kind = Character_Kind
        or else Item.Qualified_Name.To_Wide_Wide_String /= "office:body"
      loop
         Result := Events_Printers.DOM_Trees.Next_Sibling (Result);
      end loop;

      Result := Events_Printers.DOM_Trees.First_Child (Result);

      while Item.Kind = Character_Kind loop
         Result := Events_Printers.DOM_Trees.Next_Sibling (Result);
      end loop;

      pragma Assert (Item.Qualified_Name.To_Wide_Wide_String = "office:text");

      Result := Events_Printers.DOM_Trees.First_Child (Result);

      return Result;
   end Find_First_Text;

   ------------------
   -- Find_Section --
   ------------------

   function Find_Section
     (RST : Events_Printers.DOM_Trees.Tree)
      return Events_Printers.DOM_Trees.Cursor
   is
      Document : constant Events_Printers.DOM_Trees.Cursor :=
        Events_Printers.DOM_Trees.First_Child (RST.Root);
   begin
      for X in RST.Iterate_Children (Document) loop
         declare
            Item : constant Events_Printers.DOM_Element :=
              Events_Printers.DOM_Trees.Element (X);
         begin
            if Item.Kind = Element_Kind and then
              Item.Qualified_Name.To_Wide_Wide_String = "section"
            then
               return X;
            end if;
         end;
      end loop;

      raise Program_Error;
   end Find_Section;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (XML  : Events_Printers.DOM_Trees.Tree;
      Node : Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
   begin
      for Child in Events_Printers.DOM_Trees.Iterate_Subtree (Node) loop
         if XML (Child).Kind = Character_Kind then
            Result.Append (XML (Child).Lines.Join (' '));
         end if;
      end loop;

      return Result;
   end Get_Text;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Doc  : Events_Printers.DOM_Trees.Tree;
      Next : in out Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String
   is
      Cursor : Events_Printers.DOM_Trees.Cursor;
   begin
      loop
         while Events_Printers.DOM_Trees.Has_Element (Next) and then
           (Doc (Next).Kind = Character_Kind
             or else Doc (Next).Qualified_Name.To_Wide_Wide_String /= "text:h")
         loop
            Next := Events_Printers.DOM_Trees.Next_Sibling (Next);
         end loop;

         if Events_Printers.DOM_Trees.Has_Element (Next) then
            Cursor := Next;
            Next := Events_Printers.DOM_Trees.Next_Sibling (Next);

            return Get_Text (Doc, Cursor);
         else
            return League.Strings.Empty_Universal_String;
         end if;
      end loop;
   end Get_Title;

   -------------------
   -- Get_Paragraph --
   -------------------

   function Get_Paragraph
     (Doc  : Events_Printers.DOM_Trees.Tree;
      Next : in out Events_Printers.DOM_Trees.Cursor)
       return League.Strings.Universal_String
   is
      Cursor : Events_Printers.DOM_Trees.Cursor;
   begin
      loop
         while Events_Printers.DOM_Trees.Has_Element (Next) and then
           (Doc (Next).Kind = Character_Kind
            or else Doc (Next).Qualified_Name.To_Wide_Wide_String /= "text:p")
         loop
            Next := Events_Printers.DOM_Trees.Next_Sibling (Next);
         end loop;

         if Events_Printers.DOM_Trees.Has_Element (Next) then
            Cursor := Next;
            Next := Events_Printers.DOM_Trees.Next_Sibling (Next);

            if Events_Printers.DOM_Trees.Child_Count (Cursor) > 0 then
               return Get_Text (Doc, Cursor);
            end if;
         else
            return League.Strings.Empty_Universal_String;
         end if;
      end loop;
   end Get_Paragraph;

   --------------
   -- Read_Doc --
   --------------

   procedure Read_DOM
     (File   : League.Strings.Universal_String;
      Result : out Events_Printers.DOM_Trees.Tree)
   is
      Source  : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
      Reader  : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Handler : aliased Events_Printers.Events_Printer;
   begin
      Handler.Current := Handler.DOM_Tree.Root;
      Reader.Set_Feature (XML.SAX.Constants.Load_External_DTD_Feature, False);
      Reader.Set_Content_Handler (Handler'Unchecked_Access);
      Source.Open_By_File_Name (File);
      Reader.Parse (Source'Access);
      Result.Move (Handler.DOM_Tree);
   end Read_DOM;

   ------------------------
   -- Replace_Admonition --
   ------------------------

   procedure Replace_Admonition
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor) is
   begin
      for X in RST.Iterate_Children (Cursor) loop
         declare
            Child : Events_Printers.DOM_Element renames RST (X);
         begin
            if Child.Qualified_Name.To_Wide_Wide_String = "title" then
               Replace_Paragraph (RST, X, Doc, First);
            elsif Child.Qualified_Name.To_Wide_Wide_String = "paragraph" then
               Replace_Paragraph (RST, X, Doc, First);
            elsif Child.Qualified_Name.To_Wide_Wide_String =
              "literal_block"
            then
               Skip_Literal_Block (Doc, First);
            elsif Child.Qualified_Name.To_Wide_Wide_String = "bullet_list" then
               Replace_Bullet_List (RST, X, Doc, First);
            end if;
         end;
      end loop;
   end Replace_Admonition;

   -------------------------
   -- Replace_Bullet_List --
   -------------------------

   procedure Replace_Bullet_List
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor) is
   begin
      for List_Item in RST.Iterate_Children (Cursor) loop
         pragma Assert
           (RST (List_Item).Qualified_Name.To_Wide_Wide_String = "list_item");

         for Item in RST.Iterate_Children (List_Item) loop
            declare
               Name : constant Wide_Wide_String :=
                 RST (Item).Qualified_Name.To_Wide_Wide_String;
            begin
               if Name = "paragraph" then
                  Replace_Paragraph (RST, Item, Doc, First);
               elsif Name = "bullet_list" then
                  Replace_Bullet_List (RST, Item, Doc, First);
               elsif Name = "block_quote" then
                  Replace_Section (RST, Item, Doc, First);
               elsif Name = "literal_block" then
                  Skip_Literal_Block (Doc, First);
               else
                  raise Program_Error;
               end if;
            end;
         end loop;
      end loop;
   end Replace_Bullet_List;

   -----------------------
   -- Replace_Character --
   -----------------------

   procedure Replace_Character
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Text   : League.String_Vectors.Universal_String_Vector) is
   begin
      RST (Cursor).Lines := Text;
   end Replace_Character;

   -----------------------
   -- Replace_Paragraph --
   -----------------------

   procedure Replace_Paragraph
     (RST    : in out Events_Printers.DOM_Trees.Tree;
      Cursor : Events_Printers.DOM_Trees.Cursor;
      Doc    : Events_Printers.DOM_Trees.Tree;
      First  : in out Events_Printers.DOM_Trees.Cursor)
   is
      Indent : Natural := 0;
      Prev   : Events_Printers.DOM_Trees.Cursor :=
        Events_Printers.DOM_Trees.No_Element;

      Text  : League.Strings.Universal_String :=
        Get_Paragraph (Doc, First);
   begin
      for X in RST.Iterate_Children (Cursor) loop
         declare
            Item : Events_Printers.DOM_Element renames RST (X);
         begin
            if Item.Kind = Character_Kind then
               Prev := X;
            elsif Item.Qualified_Name.To_Wide_Wide_String = "target" then
               null;
            else
               declare
                  Marker : constant League.Strings.Universal_String :=
                    Get_Text (RST, X);
                  Index  : constant Natural := Text.Index (Marker);
               begin
                  if Index = 0 then
                     Ada.Wide_Wide_Text_IO.Put_Line
                       ("Marker not found: " & Marker.To_Wide_Wide_String);
                     exit;
                  elsif not Events_Printers.DOM_Trees.Has_Element (Prev)
                    and Index > 1
                  then
                     Ada.Wide_Wide_Text_IO.Put_Line
                       ("Marker at front:" & Marker.To_Wide_Wide_String);
                  end if;

                  declare
                     Lines : League.String_Vectors.Universal_String_Vector;
                  begin
                     if Index > 1 then
                        Append (Lines, Text.Head_To (Index - 1), Indent);
                        RST (Prev).Lines := Lines;
                     end if;

                     Text := Text.Tail_From (Index + Marker.Length);
                  end;
               end;
            end if;
         end;
      end loop;

      if Events_Printers.DOM_Trees.Has_Element (Prev) then
         declare
            Lines : League.String_Vectors.Universal_String_Vector;
         begin
            Append (Lines, Text, Indent);
            RST (Prev).Lines := Lines;
         end;
      end if;
   end Replace_Paragraph;

   -------------
   -- Replace --
   -------------

   procedure Replace_Section
     (RST     : in out Events_Printers.DOM_Trees.Tree;
      Section : Events_Printers.DOM_Trees.Cursor;
      Doc     : Events_Printers.DOM_Trees.Tree;
      First   : in out Events_Printers.DOM_Trees.Cursor) is
   begin
      for X in RST.Iterate_Children (Section) loop
         declare
            Child : Events_Printers.DOM_Element renames RST (X);
            Name  : constant Wide_Wide_String :=
              (if Child.Kind = Character_Kind then ""
               else Child.Qualified_Name.To_Wide_Wide_String);
         begin
            if Child.Kind = Character_Kind then
               null;
            elsif Name = "title" then
               Replace_Character
                 (RST,
                  Events_Printers.DOM_Trees.First_Child (X),
                  To_Vector (Get_Title (Doc, First)));
            elsif Name = "admonition" then
               Replace_Admonition (RST, X, Doc, First);
            elsif Name in "attention" | "hint" | "note" | "block_quote" then
               Replace_Section (RST, X, Doc, First);
            elsif Name = "paragraph" then
               Replace_Paragraph (RST, X, Doc, First);
            elsif Name = "section" then
               Replace_Section (RST, X, Doc, First);
            elsif Name = "literal_block" then
               Skip_Literal_Block (Doc, First);
            elsif Name in "bullet_list" | "enumerated_list" then
               Replace_Bullet_List (RST, X, Doc, First);
            end if;
         end;
      end loop;
   end Replace_Section;

   ------------------------
   -- Skip_Literal_Block --
   ------------------------

   procedure Skip_Literal_Block
     (Doc   : Events_Printers.DOM_Trees.Tree;
      First : in out Events_Printers.DOM_Trees.Cursor) is
   begin
      loop
         declare
            Text : constant League.Strings.Universal_String :=
             Get_Paragraph (Doc, First);
         begin
            exit when Text.Is_Empty or Text.Index ("###") > 0;
         end;
      end loop;
   end Skip_Literal_Block;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector
     (Text : League.Strings.Universal_String)
      return League.String_Vectors.Universal_String_Vector is
   begin
      return Result : League.String_Vectors.Universal_String_Vector do
         Result.Append (Text);
      end return;
   end To_Vector;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Tree    : Events_Printers.DOM_Trees.Tree;
      Cursor  : Events_Printers.DOM_Trees.Cursor;
      Handler : in out XML.SAX.Content_Handlers.SAX_Content_Handler'Class)
   is
      Success : Boolean := True;
      Item    : Events_Printers.DOM_Element renames
        Events_Printers.DOM_Trees.Element (Cursor);
   begin
      case Item.Kind is
         when Events_Printers.Character_Kind =>
            Handler.Characters (Item.Lines.Join (LF), Success);

            pragma Assert (Success);

            return;
         when Events_Printers.Element_Kind =>
            Handler.Start_Element
              (Namespace_URI => League.Strings.Empty_Universal_String,
               Local_Name     => Item.Qualified_Name,
               Qualified_Name => Item.Qualified_Name,
               Attributes     => Item.Attributes,
               Success        => Success);

            pragma Assert (Success);
      end case;

      for J in Tree.Iterate_Children (Cursor) loop
         Traverse (Tree, J, Handler);
      end loop;

      Handler.End_Element
        (Namespace_URI  => League.Strings.Empty_Universal_String,
         Local_Name     => Item.Qualified_Name,
         Qualified_Name => Item.Qualified_Name,
         Success        => Success);

      pragma Assert (Success);
   end Traverse;

   Doc      : Events_Printers.DOM_Trees.Tree;
   RST      : Events_Printers.DOM_Trees.Tree;
   Output   : aliased
     XML.SAX.String_Output_Destinations.String_Output_Destination;
   Unparser : aliased RST_Unparsers.RST_Unparser (Output'Access);
   First    : Events_Printers.DOM_Trees.Cursor;
begin
   Read_DOM (League.Application.Arguments.Element (1), RST);
   Read_DOM (League.Application.Arguments.Element (3), Doc);

   RST_Unparsers.Read_URI_Map
     (Unparser, League.Application.Arguments.Element (2));

   declare
      Ok : Boolean := True;
   begin
      Unparser.Start_Document (Ok);
   end;

   First := Find_First_Text (Doc);

   Replace_Section
     (RST,
      Find_Section (RST),
      Doc,
      First);

   Traverse
     (RST,
      Events_Printers.DOM_Trees.First_Child (RST.Root),
      Unparser);

   Ada.Wide_Wide_Text_IO.Put_Line (Output.Get_Text.To_Wide_Wide_String);
end Replace;
