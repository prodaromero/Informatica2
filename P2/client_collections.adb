with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Lower_Layer_UDP;
with Ada.Characters.Handling;
with Chat_Messages;

package body Client_Collections is

    package T_IO renames Ada.Text_IO;
    package CH renames Ada.Characters.Handling;
    package CM renames Chat_Messages;

--------------------------------COMPROBAR LISTA---------------------------------
    function Empty_Collection (Collection: Cell_A) return Boolean is
    begin
        return Collection = null;
    end Empty_Collection;

--------------------------------LIBERAR MEMORIA---------------------------------
    procedure Free is
        new Ada.Unchecked_Deallocation(Cell, Cell_A);

-------------------------------COMPARAR PALABRAS--------------------------------
    function To_Compare(Nick_1: ASU.Unbounded_String;
                        Nick_2: ASU.Unbounded_String) return Boolean is
    begin -- To_Compare
        return CH.To_Lower(ASU.To_String(Nick_1)) = CH.To_Lower(ASU.To_String(Nick_2));
    end To_Compare;

----------------------------------SEPARATE EP-----------------------------------
    procedure Separate_EP(EP: in LLU.End_Point_Type;
                            Users: in out ASU.Unbounded_String) is
        Position: Natural;
        IP: ASU.Unbounded_String;
        Port: ASU.Unbounded_String;
    begin
        Users := ASU.To_Unbounded_String(LLU.Image(EP));
        Users := ASU.Tail(Users, ASU.Length(Users) - 37); --192.247.49.72, Port: 1023
        Position := ASU.Index(Users,",");
        IP := ASU.Head(Users, Position-1); -- 192.247.49.72
        ASU.Tail(Users, ASU.Length(Users)-Position); -- _Port: 1023
        Port := ASU.Tail(Users, ASU.Length(Users) - 8); -- 1023

        Users := ASU.To_Unbounded_String(ASU.To_String(IP) & ":" &
                                        ASU.To_String(Port));
    end Separate_EP;

----------------------------------ADD CLIENT------------------------------------
    procedure Add_Client (Collection: in out Collection_Type;
                          EP: in LLU.End_Point_Type;
                          Nick: in ASU.Unbounded_String;
                          Unique: in Boolean) is

        P_Nodo: Cell_A;
        P_Aux: Cell_A;
    begin -- Add_Client
        P_Aux := new Cell;
        P_Aux.Client_EP := EP;
        P_Aux.Nick := Nick;

        if Unique then -- Unique = True ==> Clien: writer
            if not Empty_Collection(Collection.P_First) then
                P_Nodo := Collection.P_First;
                if not To_Compare(P_Nodo.Nick, P_Aux.Nick) then
                    if not Empty_Collection(P_Nodo.Next) then
                        loop
                            P_Nodo := P_Nodo.Next;
                            exit when Empty_Collection(P_Nodo.Next) or
                                To_Compare(P_Nodo.Nick, P_Aux.Nick);
                        end loop;

                        if not To_Compare(P_Nodo.Nick, P_Aux.Nick) then
                            P_Aux.Next := Collection.P_First;
                            Collection.P_First := P_Aux;
                            Collection.Total := Collection.Total + 1;
                            P_Aux := null;
                        else
                            raise Client_Collection_Error;
                        end if;
                    else
                        P_Aux.Next := Collection.P_First;
                        Collection.P_First := P_Aux;
                        Collection.Total := Collection.Total + 1;
                        P_Aux := null;
                    end if;
                else
                    raise Client_Collection_Error;
                end if;
            else
                Collection.P_First := P_Aux;
                Collection.Total := Collection.Total + 1;
                P_Aux := null;
            end if;
        else -- Unique = False ==> Client: reader
            if not Empty_Collection(Collection.P_First) then
                P_Aux.Next :=Collection.P_First;
                Collection.P_First := P_Aux;
                Collection.Total := Collection.Total + 1;
                P_Aux := null;
            else
                Collection.P_First := P_Aux;
                Collection.Total := Collection.Total + 1;
                P_Aux := null;
            end if;
        end if;
    T_IO.Put_Line("INIT received from " & ASU.To_String(Nick));
    exception
        when Client_Collection_Error =>
            T_IO.Put_Line("INIT received from " &
                        ASU.To_String(Nick) & ". IGNORED, nick already used");
    end Add_Client;

--------------------------------DELETE CLIENT-----------------------------------
    procedure Delete_Client (Collection: in out Collection_Type;
                             Nick: in ASU.Unbounded_String) is
        P_Nodo: Cell_A;
        P_Aux: Cell_A;
    begin
        if not Empty_Collection(Collection.P_First) then
            P_Nodo := Collection.P_First;
            if To_Compare(Nick, P_Nodo.Nick) then
                Collection.P_First := P_Nodo.Next;
                P_Nodo.Next := null;
                Free(P_Nodo);
            else
                loop
                    exit when Empty_Collection(P_Nodo.Next) or
                        To_Compare(Nick, P_Nodo.Nick);
                    P_Aux := P_Nodo;
                    P_Nodo := P_Nodo.Next;
                end loop;

                if To_Compare(Nick, P_Nodo.Nick) then
                    P_Aux.Next := P_Nodo.Next;
                    P_Nodo.Next := null;
                    Free(P_Nodo);
                else
                    raise Client_Collection_Error;
                end if;
            end if;
        else
            raise Client_Collection_Error;
        end if;
    end Delete_Client;

--------------------------------SEARCH CLIENT-----------------------------------
     function Search_Client (Collection: in Collection_Type;
                             EP: in LLU.End_Point_Type)
                             return ASU.Unbounded_String is
        P_Nodo: Cell_A;
        Nick: ASU.Unbounded_String;
     begin
        if not Empty_Collection(Collection.P_First) then
            P_Nodo := Collection.P_First;
            if LLU.Image(P_Nodo.Client_EP) = LLU.Image(EP) then
                Nick := P_Nodo.Nick;
            else
                loop
                    exit when Empty_Collection(P_Nodo.Next) or
                            LLU.Image(P_Nodo.Client_EP) = LLU.Image(EP);
                    P_Nodo := P_Nodo.Next;
                end loop;

                if LLU.Image(P_Nodo.Client_EP) = LLU.Image(EP) then
                    Nick := P_Nodo.Nick;
                else
                    raise Client_Collection_Error;
                end if;
            end if;
        else
            raise Client_Collection_Error;
        end if;

        return Nick;
     end Search_Client;

------------------------------SEND TO ALL CLIENT--------------------------------
    procedure Send_To_All (Collection: in Collection_Type;
                           P_Buffer: access LLU.Buffer_Type) is
        P_Nodo: Cell_A;
    begin
        if not Empty_Collection(Collection.P_First) then
            P_Nodo := Collection.P_First;
            loop
                LLU.Send(P_Nodo.Client_EP, P_Buffer);
                P_Nodo := P_Nodo.Next;
                exit when Empty_Collection(P_Nodo);
            end loop;
        end if;
    end Send_To_All;

-------------------------------COLLECTION IMAGE---------------------------------
    function Collection_Image (Collection: in Collection_Type)
                               return String is
        P_Nodo: Cell_A;
        Users: ASU.Unbounded_String;
        New_User: ASU.Unbounded_String;
        Nick: ASU.Unbounded_String;
    begin
        P_Nodo := Collection.P_First;
        if not Empty_Collection(Collection.P_First) then
            Separate_EP(P_Nodo.Client_EP, Users);
            Nick := P_Nodo.Nick;
            Users := ASU.To_Unbounded_String(ASU.To_String(Users) & " " &
                                            ASU.To_String(Nick));

            while not Empty_Collection(P_Nodo.Next) loop
                P_Nodo := P_Nodo.Next;
                Separate_EP(P_Nodo.Client_EP, New_User);
                Nick := P_Nodo.Nick;
                New_User := ASU.To_Unbounded_String(ASU.To_String(New_User)
                                & " " & ASU.To_String(Nick));
                Users := ASU.To_Unbounded_String(ASU.To_String(Users) &
                                            ASCII.LF & ASU.To_String(New_User));
            end loop;
        else
            raise Client_Collection_Error;
        end if;

        return ASU.To_String(Users);
    end Collection_Image;

end Client_Collections;
