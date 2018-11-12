with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Chat_Messages;

procedure Chat_Client is
    package LLU  renames Lower_Layer_UDP;
    package ASU  renames Ada.Strings.Unbounded;
    package T_IO renames Ada.Text_IO;
    package ACL  renames Ada.Command_Line;
    package CM   renames Chat_Messages;
    use type CM.Messages_Type;

    Server_EP:  LLU.End_Point_Type;
    Client_EP:  LLU.End_Point_Type;
    Host:       ASU.Unbounded_String;
    Dir_IP:     ASU.Unbounded_String;
    Port:       ASU.Unbounded_String;
    Nickname:   ASU.Unbounded_String;
    Buffer:     aliased LLU.Buffer_Type(1024);
    Request:    ASU.Unbounded_String;
    Reply:      ASU.Unbounded_String;
    Mess:       CM.Messages_Type;

    Usage_Error: exception;

begin

---------------------------CONTROL OF INPUT PARAMETERS--------------------------
    if ACL.Argument_Count = 3 then
        Host := ASU.To_Unbounded_String(ACL.Argument(1));
        Port := ASU.To_Unbounded_String(ACL.Argument(2));
        Nickname := ASU.To_Unbounded_String(ACL.Argument(3));
    else
        raise Usage_Error;
    end if;

--------------------------------BUILD CONNECTION--------------------------------
    Dir_IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
    -- Construye el End_Point en el que está atado el servidor
    Server_EP := LLU.Build(ASU.To_String(Dir_IP), Integer'Value(ASU.To_String(Port)));
    -- Construye un End_Point libre cualquiera y se ata a él
    LLU.Bind_Any(Client_EP);

-------------------------------BUILD INIT MESSAGE-------------------------------
    Mess := CM.Init;
    LLU.Reset(Buffer);
    CM.Messages_Type'Output(Buffer'Access, Mess);
    LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
    ASU.Unbounded_String'Output(Buffer'Access, Nickname);

    LLU.Send(Server_EP, Buffer'Access);


    loop
        if ASU.To_String(Nickname) = "reader" then
-------------------------------CASE READER CLIENT-------------------------------
            loop
                LLU.Reset(Buffer);

                -- Receive server message
                LLU.Receive(Client_EP, Buffer'Access);

                Mess := CM.Messages_Type'Input(Buffer'Access);
                Nickname := ASU.Unbounded_String'Input(Buffer'Access);
                Reply := ASU.Unbounded_String'Input(Buffer'Access);

                T_IO.Put_Line(ASU.To_String(Nickname) & ": " &
                            ASU.To_String(Reply));
            end loop;
        else
-------------------------------CASE WRITER CLIENT-------------------------------
            Ada.Text_IO.Put("Message: ");
            Request := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);

            exit when ASU.To_String(Request) = ".quit";

            -- Build writer message
            LLU.Reset(Buffer);
            Mess := CM.Writer;
            CM.Messages_Type'Output(Buffer'Access, Mess);
            LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
            ASU.Unbounded_String'Output(Buffer'Access, Request);

            -- Send message to server
            LLU.Send(Server_EP, Buffer'Access);
        end if;
    end loop;

    LLU.Finalize;

exception
    when Usage_Error =>
        T_IO.Put_Line("Usage: ./chat_client <host name> <port> <nickname>");
        LLU.Finalize;
    when Ex:others =>
        T_IO.Put_Line ("Excepción imprevista: " &
                             Ada.Exceptions.Exception_Name(Ex) & " en: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;
end Chat_Client;
