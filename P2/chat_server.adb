with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Chat_Messages;
with Client_Collections;

procedure Chat_Server is
    package LLU  renames Lower_Layer_UDP;
    package ASU  renames Ada.Strings.Unbounded;
    package T_IO renames Ada.Text_IO;
    package ACL  renames Ada.Command_Line;
    package CC   renames Client_Collections;
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
    Empty:      Boolean := True;
    Found:      Boolean := True;


    Writer_Collection: CC.Collection_Type;
    Reader_Collection: CC.Collection_Type;

    Usage_Error:             exception;
    Client_Collection_Error: exception;
    Bad_Request_Error:       exception;

begin
---------------------------CONTROL OF INPUT PARAMETERS--------------------------
    if ACL.Argument_Count = 1 then
        Port := ASU.To_Unbounded_String(ACL.Argument(1));
    else
        raise Usage_Error;
    end if;

----------------------------------BUILD SERVER----------------------------------
    Host := ASU.To_Unbounded_String(LLU.Get_Host_Name);
    Dir_IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));

    -- construye un End_Point en una dirección y puerto concretos
    Server_EP := LLU.Build(ASU.To_String(Dir_IP), Integer'Value(ASU.To_String(Port)));
    -- se ata al End_Point para poder recibir en él
    LLU.Bind(Server_EP);

    T_IO.Put_Line("Server " & ASU.To_String(Host) & " listening at port " &
                    ASU.To_String(Port) & " ...");
    T_IO.New_Line(1);

    loop -- Wait for messages
        LLU.Reset(Buffer);

        LLU.Receive(Server_EP, Buffer'Access);

        Mess := CM.Messages_Type'Input(Buffer'Access);

----------------------------------INIT MESSAGE----------------------------------
        case Mess is
            when CM.Init => -- Init message control
                begin
                    Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
                    Nickname := ASU.Unbounded_String'Input(Buffer'Access);

                    -- In case of Reader client => Add to Reader_Collection
                    if ASU.To_String(Nickname) = "reader" then
                        CC.Add_Client(Reader_Collection, Client_EP, Nickname, False);
                        Empty := False;
                    else
                    -- In case of Writer client => Add to Writer_Collection
                        CC.Add_Client(Writer_Collection, Client_EP, Nickname, True);

                        LLU.Reset(Buffer);
                        Mess := CM.Server;

                        Found := True;
                        Nickname := CC.Search_Client(Writer_Collection, Client_EP);

                        -- Send the Init message to Readers clients
                        if not Empty and Found then
                            Reply := ASU.To_Unbounded_String(ASU.To_String(Nickname)
                                        & " joins the chat");
                            CM.Messages_Type'Output(Buffer'Access, Mess);
                            ASU.Unbounded_String'Output(Buffer'Access,
                                            ASU.To_Unbounded_String("server"));
                            ASU.Unbounded_String'Output(Buffer'Access, Reply);
                            CC.Send_To_All(Reader_Collection, Buffer'Access);
                        end if;
                    end if;
                exception -- In case of repeat writer client => Not send
                    when CC.Client_Collection_Error =>
                        Found := False;
                end;
            when CM.Writer => -- Writer message control
                begin
                    Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
                    Request := ASU.Unbounded_String'Input(Buffer'Access);

                    Nickname := CC.Search_Client(Writer_Collection, Client_EP);

                    T_IO.Put_Line("WRITER received from " & ASU.To_String(Nickname)
                                    & ": " & ASU.To_String(Request));

                    LLU.Reset(Buffer);

                    -- Build the request and send to all Readers clients
                    Mess := CM.Server;
                    CM.Messages_Type'Output(Buffer'Access, Mess);
                    ASU.Unbounded_String'Output(Buffer'Access, Nickname);
                    ASU.Unbounded_String'Output(Buffer'Access, Request);

                    if not Empty then
                        CC.Send_To_All(Reader_Collection, Buffer'Access);
                    end if;
                exception -- In case of repeat client => Ignored
                    when CC.Client_Collection_Error =>
                        T_IO.Put_Line("WRITER received from unknown client. IGNORED");
                end;
            when others =>
                raise Bad_Request_Error;
        end case;
    end loop;

exception
    when Usage_Error =>
        T_IO.Put_Line("Usage: ./chat_server <port>");
        LLU.Finalize;
    when Bad_Request_Error =>
        T_IO.Put_Line("400 Bad Request");
    when Ex:others =>
        T_IO.Put_Line ("Excepción imprevista: " &
                            Ada.Exceptions.Exception_Name(Ex) & " en: " &
                            Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;
end Chat_Server;
