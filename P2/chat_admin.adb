with Lower_Layer_UDP;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Chat_Messages;

procedure Chat_Admin is
    package LLU  renames Lower_Layer_UDP;
    package ASU  renames Ada.Strings.Unbounded;
    package T_IO renames Ada.Text_IO;
    package ACL  renames Ada.Command_Line;
    package CM   renames Chat_Messages;
    use type CM.Messages_Type;

    Server_EP:  LLU.End_Point_Type;
    Admin_EP:   LLU.End_Point_Type;
    Host:       ASU.Unbounded_String;
    Dir_IP:     ASU.Unbounded_String;
    Port:       ASU.Unbounded_String;
    Password:   ASU.Unbounded_String;
    Buffer:     aliased LLU.Buffer_Type(1024);
    Request:    ASU.Unbounded_String;
    Mess:       CM.Messages_Type;
    Option:     Integer;
    Nick:       ASU.Unbounded_String;
    Expired:    Boolean;

    Usage_Error:             exception;
    Option_Error:            exception;
    Client_Collection_Error: exception;


-------------------------------MENU INTERACTIVO--------------------------------
    procedure Launch_Menu is

    begin -- Launch_Menu
        T_IO.New_Line(1);
        T_IO.Put_Line("Options");
        T_IO.Put_Line("1 Show writers collection");
        T_IO.Put_Line("2 Ban writer");
        T_IO.Put_Line("3 Shutdown server");
        T_IO.Put_Line("4 Quit");
        T_IO.New_Line(1);
        T_IO.Put("Your option? ");
    end Launch_Menu;

    procedure Interactive_Menu is
    begin -- Interactive_Menu
        loop
            Launch_Menu;
            Option := Integer'Value(T_IO.Get_Line);

            case Option is
                when 1 => -- Show writers collection
                    -- Send Collection_Request message
                    Mess := CM.Collection_Request;
                    LLU.Reset(Buffer);
                    CM.Messages_Type'Output(Buffer'Access, Mess);
                    LLU.End_Point_Type'Output(Buffer'Access, Admin_EP);
                    ASU.Unbounded_String'Output(Buffer'Access, Password);

                    LLU.Send(Server_EP, Buffer'Access);

                    -- Receive Collection_Data message
                    LLU.Receive(Admin_EP, Buffer'Access, 5.0, Expired);

                    if not Expired then
                        Mess := CM.Messages_Type'Input(Buffer'Access);
                        Request := ASU.Unbounded_String'Input(Buffer'Access);
                    else
                        raise Client_Collection_Error;
                    end if;

                    T_IO.New_Line(1);
                    T_IO.Put_Line(ASU.To_String(Request));

                when 2 => -- Ban writer

                  T_IO.Put("Nick to ban? ");
                  Nick := ASU.To_Unbounded_String(T_IO.Get_Line);
                  -- Send Ban message;
                  Mess := CM.Ban;
                  LLU.Reset(Buffer);
                  CM.Messages_Type'Output(Buffer'Access, Mess);
                  ASU.Unbounded_String'Output(Buffer'Access, Password);
                  ASU.Unbounded_String'Output(Buffer'Access, Nick);

                  LLU.Send(Server_EP, Buffer'Access);

                when 3 => -- Shutdown message
                    Mess := CM.Shutdown;
                    LLU.Reset(Buffer);
                    CM.Messages_Type'Output(Buffer'Access, Mess);
                    ASU.Unbounded_String'Output(Buffer'Access, Password);

                    LLU.Send(Server_EP, Buffer'Access);

                    T_IO.Put_Line("Server shutdown sent");

                when 4 => -- Quit
                    LLU.Finalize;
                when others =>
                    raise Option_Error;
            end case;
            exit when Option = 4;
        end loop;
    end Interactive_Menu;

begin -- Chat_Admin

    if ACL.Argument_Count = 3 then
        Host := ASU.To_Unbounded_String(ACL.Argument(1));
        Port := ASU.To_Unbounded_String(ACL.Argument(2));
        Password := ASU.To_Unbounded_String(ACL.Argument(3));
    else
        raise Usage_Error;
    end if;

--------------------------------BUILD CONNECTION--------------------------------
    Dir_IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
    -- Construye el End_Point en el que está atado el servidor
    Server_EP := LLU.Build(ASU.To_String(Dir_IP), Integer'Value(ASU.To_String(Port)));
    -- Construye un End_Point libre cualquiera y se ata a él
    LLU.Bind_Any(Admin_EP);

    loop
        Interactive_Menu;

        exit when Option = 4;
    end loop;

    LLU.Finalize;

exception
    when Usage_Error =>
        T_IO.Put_Line("Usage: ./chat_admin <host name> <port> <password>");
        LLU.Finalize;
    when Client_Collection_Error =>
        T_IO.Put_Line("Incorrect Password");
        LLU.Finalize;
    when Option_Error | Constraint_Error  =>
        T_IO.Put_Line("Invalid option. Please, try again");
        Chat_Admin;
    when Ex:others =>
        T_IO.Put_Line ("Excepción imprevista: " &
                             Ada.Exceptions.Exception_Name(Ex) & " en: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;
end Chat_Admin;
