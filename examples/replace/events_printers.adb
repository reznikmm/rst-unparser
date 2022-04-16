--  SPDX-FileCopyrightText: 2022 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

package body Events_Printers is

   procedure Append_Characters (Self : in out Events_Printer'Class);

   -----------------------
   -- Append_Characters --
   -----------------------

   procedure Append_Characters (Self : in out Events_Printer'Class) is
   begin
      if Self.Lines.Is_Empty then
         return;
      end if;

      declare
         Lines : constant League.String_Vectors.Universal_String_Vector :=
           Self.Lines.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
         Node  : constant DOM_Element := (Character_Kind, Lines);
      begin
         Self.DOM_Tree.Insert_Child
           (Self.Current,
            DOM_Trees.No_Element,
            Node);

         Self.Lines.Clear;
      end;
   end Append_Characters;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out Events_Printer;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean) is
      pragma Unreferenced (Success);
   begin
      Self.Lines.Append (Text);
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self           : in out Events_Printer;
      Namespace_URI  :        League.Strings.Universal_String;
      Local_Name     :        League.Strings.Universal_String;
      Qualified_Name :        League.Strings.Universal_String;
      Success        : in out Boolean) is
      pragma Unreferenced (Success);
   begin
      Self.Append_Characters;
      Self.Current := DOM_Trees.Parent (Self.Current);
   end End_Element;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   overriding procedure End_Prefix_Mapping
     (Self   : in out Events_Printer;
      Prefix : League.Strings.Universal_String;
      Success : in out Boolean)
   is
   begin
      null;
   end End_Prefix_Mapping;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : Events_Printer) return League.Strings.Universal_String
   is
   begin
      return raise Program_Error with "Unimplemented function Error_String";
   end Error_String;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Self    : in out Events_Printer; Text : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      null;
   end Ignorable_Whitespace;

   ----------------------------
   -- Processing_Instruction --
   ----------------------------

   overriding procedure Processing_Instruction
     (Self : in out Events_Printer; Target : League.Strings.Universal_String;
      Data :        League.Strings.Universal_String; Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      null;
   end Processing_Instruction;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   overriding procedure Set_Document_Locator
     (Self : in out Events_Printer; Locator : XML.SAX.Locators.SAX_Locator)
   is
   begin
      Self.Locator := Locator;
   end Set_Document_Locator;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self           : in out Events_Printer;
      Namespace_URI  :        League.Strings.Universal_String;
      Local_Name     :        League.Strings.Universal_String;
      Qualified_Name :        League.Strings.Universal_String;
      Attributes     : XML.SAX.Attributes.SAX_Attributes;
      Success        : in out Boolean)
   is
      pragma Unreferenced (Success);
      Element : DOM_Element (Element_Kind);
      Current : DOM_Trees.Cursor;
   begin
      Self.Append_Characters;
      Element.Qualified_Name := Qualified_Name;
      Element.Attributes := Attributes;

      Self.DOM_Tree.Insert_Child
        (Self.Current,
         DOM_Trees.No_Element,
         Element,
         Current);

      Self.Current := Current;
   end Start_Element;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   overriding procedure Start_Prefix_Mapping
     (Self   : in out Events_Printer;
      Prefix : League.Strings.Universal_String;
      Namespace_URI :        League.Strings.Universal_String;
      Success       : in out Boolean)
   is
   begin
      null;
   end Start_Prefix_Mapping;

end Events_Printers;
