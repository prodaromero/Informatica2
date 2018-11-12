with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Word_Lists;

procedure Words is
-------------------------------RENOMBRAR PAQUETES-------------------------------
    package ACL renames Ada.Command_Line;
    package ASU renames Ada.Strings.Unbounded;
    package T_IO renames Ada.Text_IO;
    package WL renames Word_Lists;

----------------------------------VARIABLES-------------------------------------
    File_Name:  ASU.Unbounded_String;
    File:       T_IO.File_Type;
    Line:       ASU.Unbounded_String;
    Word:       ASU.Unbounded_String;
    Finish:     Boolean;
    Count:      Integer;

    Menu: ASU.Unbounded_String;

    P_Lista: WL.Word_List_Type;

    Usage_Error: exception;
    Name_Error: exception;

-------------------------------SEPARAR PALABRAS---------------------------------
    procedure Separate_Words (Line: in out ASU.Unbounded_String;
                              Word: out ASU.Unbounded_String;
                              Delimiter: in String;
                              P_Lista: in out WL.Word_List_Type) is
        Position: Integer;
    begin -- Separate_Words

        loop

            Position := ASU.Index(Line, Ada.Strings.Maps.To_Set(Delimiter));

            if Position = 1 then
                ASU.Tail(Line, ASU.Length(Line) - 1);
            elsif Position /= 0 then
                Word := ASU.Head(Line, Position - 1);
                ASU.Tail(Line, ASU.Length(Line) - Position);
                WL.Add_Word(P_Lista, Word);
            elsif ASU.Length(Line) /= 0 and ASU.To_String(Line) /= Delimiter then
                WL.Add_Word(P_Lista, Line);
            end if;
            exit when Position = 0;

        end loop;

    end Separate_Words;

--------------------------------MENU INTERACTIVO--------------------------------
    procedure Launch_Menu is

    begin -- Launch_Menu
        T_IO.New_Line(1);
        T_IO.Put_Line("Options");
        T_IO.Put_Line("1 Add word");
        T_IO.Put_Line("2 Delete word");
        T_IO.Put_Line("3 Search word");
        T_IO.Put_Line("4 Show all words");
        T_IO.Put_Line("5 Quit");
        T_IO.New_Line(1);
        T_IO.Put_Line("Your option? ");

    end Launch_Menu;

    procedure Interactive_Menu(List: in out WL.Word_List_Type) is

        Option: Integer;
        Word: ASU.Unbounded_String;
        Count: Integer;
    begin -- Interactive_Menu
        loop
            Launch_Menu;
            Option := Integer'Value(T_IO.Get_Line);

            case Option is
                when 1 =>
                    T_IO.Put_Line("Word? ");
                    Word := ASU.To_Unbounded_String(T_IO.Get_Line);

                    WL.Add_Word(List, Word);
                    T_IO.New_Line(1);
                    T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| added");

                when 2 =>
                    T_IO.Put_Line("Word? ");
                    Word := ASU.To_Unbounded_String(T_IO.Get_Line);

                    WL.Delete_Word(List, Word);
                    T_IO.New_Line(1);
                    T_IO.Put_Line("Word |" & ASU.To_String(Word) & "| deleted");

                when 3 =>
                    T_IO.Put_Line("Word? ");
                    Word := ASU.To_Unbounded_String(T_IO.Get_Line);

                    WL.Search_Word(List, Word, Count);
                    T_IO.New_Line(1);
                    T_IO.Put_Line(" |" & ASU.To_String(Word) & "| - " &
                                    Integer'Image(Count));
                when 4 =>
                    T_IO.New_Line(1);
                    WL.Print_All(List);
                when 5 =>
                    T_IO.New_Line(1);
                    WL.Max_Word(List, Word, Count);
                    T_IO.Put_Line(" |" & ASU.To_String(Word) & "| - " &
                                    Integer'Image(Count));
                    WL.Delete_List(List);
                when others =>
                    T_IO.Put_Line("Sorry, your options is not valid. Plese, try again.");
            end case;
            exit when Option = 5;

        end loop;
    end Interactive_Menu;

----------------------------------CONTOL FICHERO--------------------------------
    function File_Exists (Name: String) return Boolean is
        File: T_IO.File_Type;
    begin -- File_Exists
        T_IO.Open(File, T_IO.In_File, Name);
        T_IO.Close(File);
        return True;
    exception
        when Ada.IO_Exceptions.Name_Error =>
            return False;
    end File_Exists;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          PROGRAMA PRINCIPAL                                --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
begin

----------------------------------CONTOL ENTRADA--------------------------------
    if ACL.Argument_Count = 1 then
        File_Name := ASU.To_Unbounded_String(ACL.Argument(1));
    elsif ACL.Argument_Count = 2 then
        Menu := ASU.To_Unbounded_String(ACL.Argument(1));
        File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
    else
        raise Usage_Error;
    end if;

----------------------------COMPROBAR TEXTO----------------------------
    if File_Exists(ASU.To_String(File_Name)) then
        T_IO.Open(File, T_IO.In_File, ASU.To_String(File_Name));

        Finish := False;

----------------------------LEER TEXTO Y ALMACENARLO----------------------------

        while not Finish loop
            begin
                Line := ASU.To_Unbounded_String(T_IO.Get_Line(File));
                Separate_Words(Line, Word, " ,.-", P_Lista);
            exception
                when Ada.IO_Exceptions.End_Error =>
                    Finish := True;
            end;
        end loop;

        if ASU.To_String(Menu) = "-i" then
            Interactive_Menu(P_Lista);
        else
            WL.Max_Word(P_Lista, Word, Count);
            T_IO.Put_Line("The most frequent word: |" & ASU.To_String(Word) &
                            "| - " & Integer'Image(Count));
        end if;

        T_IO.Close(File);
    else
        raise Name_Error;
    end if;


exception
    when Usage_Error =>
        T_IO.New_Line(1);
        T_IO.Put_Line("usage: words [-i] <filename>" );
    when WL.Word_List_Error =>
        T_IO.New_Line(1);
        T_IO.Put_Line("Your word is not exit.");
    when Name_Error =>
        T_IO.Put_Line(ASU.To_String(File_Name) & ": File not found");
end Words;
