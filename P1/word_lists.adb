with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;

package body Word_Lists is

    package T_IO renames Ada.Text_IO;
    package CH renames Ada.Characters.Handling;
    
--------------------------------COMPROBAR LISTA---------------------------------
    function Empty_List (List: Word_List_Type) return Boolean is
    begin
        return List = null;
    end Empty_List;

--------------------------------LIBERAR MEMORIA---------------------------------
    procedure Free is
        new Ada.Unchecked_Deallocation(Cell, Word_List_Type);

-------------------------------COMPARAR PALABRAS--------------------------------
    function To_Compare(Word_1: ASU.Unbounded_String;
                        Word_2: ASU.Unbounded_String) return Boolean is

    begin -- To_Compare
        return CH.To_Lower(ASU.To_String(Word_1)) = CH.To_Lower(ASU.To_String(Word_2));
    end To_Compare;

--------------------------------AÑADIR PALABRA----------------------------------
    procedure Add_Word (List: in out Word_List_Type;
 		                Word: in ASU.Unbounded_String) is

        P_Nodo: Word_List_Type;
        P_Aux: Word_List_Type;
    begin -- Add_Word
        P_Aux := new Cell;
        P_Aux.Word := Word;

        if not Empty_List(List) then
            P_Nodo := List;
            if To_Compare(P_Aux.Word, List.Word) then
                List.Count := List.Count + 1;
            elsif not Empty_List(List.Next) then
                loop
                    P_Nodo := P_Nodo.Next;
                    exit when Empty_List(P_Nodo.Next) or
                        To_Compare(P_Nodo.Word, P_Aux.Word);
                end loop;

                if To_Compare(P_Aux.Word, P_Nodo.Word) then
                    P_Nodo.Count := P_Nodo.Count + 1;
                else
                    P_Nodo.Next := P_Aux;
                    P_Aux.Count := 1;
                    P_Aux := null;
                    Free(P_Aux);
                end if;
            elsif Empty_List(List.Next) then
                P_Nodo.Next := P_Aux;
                P_Aux.Count := 1;
                P_Aux := null;
                Free(P_Aux);
            end if;
        else
            List := P_Aux;
            List.Count := 1;
            P_Aux := null;
            Free(P_Aux);
        end if;
    end Add_Word;

--------------------------------BORRAR PALABRAS---------------------------------
    procedure Delete_Word (List: in out Word_List_Type;
 			                 Word: in ASU.Unbounded_String) is
        P_Nodo: Word_List_Type;
        P_Aux: Word_List_Type;
        P_Ant: Word_List_Type;
    begin -- Delete_Word
        P_Aux := new Cell;
        P_Aux.Word := Word;

        if not Empty_List(List) then
            P_Nodo := List;
            -- Controlamos la primera "celda" de la lista
            if To_Compare(P_Nodo.Word, P_Aux.Word) then
                if Empty_List(List.Next) then -- Si solo hay un elemento
                    Free(List);
                    Free(P_Aux);
                else                          -- Si hay más de un elemento
                    List := List.Next;
                    Free(P_Nodo);
                end if;
                                                 -- Si no esta en la primera celda
            elsif not Empty_List(List.Next) then -- comprobamos en el resto de celdas
                loop
                    P_Ant := P_Nodo;
                    P_Nodo := P_Nodo.Next;
                    exit when Empty_List(P_Nodo.Next) or
                        To_Compare(P_Nodo.Word, P_Aux.Word);
                end loop;
                if To_Compare(P_Nodo.Word, P_Aux.Word) then
                    if Empty_List(P_Nodo.Next) then
                        P_Ant.Next := null;
                        Free(P_Nodo);
                        Free(P_Aux);
                    else
                        P_Ant.Next := P_Nodo.Next;
                        P_Nodo.Next := null;
                        Free(P_Nodo);
                        Free(P_Aux);
                    end if;
                else
                    raise Word_List_Error;
                end if;
            end if;
        else
            raise Word_List_Error;
        end if;
    end Delete_Word;

--------------------------------AÑADIR PALABRA----------------------------------
    procedure Search_Word (List: in Word_List_Type;
                              Word: in ASU.Unbounded_String;
                              Count: out Natural) is
        P_Nodo: Word_List_Type;
        P_Aux: Word_List_Type;
    begin -- Search_Word;
        P_Nodo := List;
        P_Aux := new Cell;
        P_Aux.Word := Word;

        if To_Compare(P_Nodo.Word, P_Aux.Word) then
            Count := List.Count;
        else
            loop
                P_Nodo := P_Nodo.Next;
                exit when Empty_List(P_Nodo.Next) or
                    To_Compare(P_Nodo.Word, P_Aux.Word);
            end loop;

            if To_Compare(P_Nodo.Word, P_Aux.Word) then
                Count := P_Nodo.Count;
            else
                Count := 0;
            end if;
        end if;
    end Search_Word;

--------------------------------MENU INTERACTIVO--------------------------------
    procedure Max_Word (List: in Word_List_Type;
                         Word: out ASU.Unbounded_String;
                         Count: out Natural) is

        P_Nodo: Word_List_Type;
        P_Aux: Word_List_Type;
    begin -- Max_Word
        P_Nodo := List;
        P_Aux := new Cell;
        P_Aux.Word := Word;

        if not Empty_List(List) then
            loop
                if P_Nodo.Count > P_Aux.Count then
                    P_Aux.Word := P_Nodo.Word;
                    P_Aux.Count := P_Nodo.Count;
                else
                    P_Nodo := P_Nodo.Next;
                end if;
                Word := P_Aux.Word;
                Count := P_Aux.Count;
                exit when Empty_List(P_Nodo.Next);
            end loop;
        else
            raise Word_List_Error;

        end if;
    end Max_Word;

-------------------------------IMPRIMIR PALABRAS--------------------------------
    procedure Print_All(List: in Word_List_Type) is
        P_Aux: Word_List_Type;
    begin -- Print_All
        P_Aux := List;
        if Empty_List(P_Aux) then
            T_IO.Put_Line("No words.");
        end if;
        while not Empty_List(P_Aux) loop
            T_IO.Put_Line(" |" & ASU.To_String(P_Aux.Word) & "| - " &
                          Integer'Image(P_Aux.Count));
            P_Aux := P_Aux.Next;
        end loop;
    end Print_All;

    procedure Delete_List (List: in out Word_List_Type) is
        P_Nodo: Word_List_Type;
    begin
        while not Empty_List(List) loop
            if not Empty_List(List.Next) then
                P_Nodo := List;
                List := List.Next;
                Free(P_Nodo);
            else
                Free(List);
            end if;
        end loop;
    end Delete_List;

end Word_Lists;
