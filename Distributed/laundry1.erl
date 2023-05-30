%% <Ayman Makhoukhi>
%% <OICF7P>
%% <DDS, Distributed Retake>
%% <16.05.2023>
%% This solution was submitted and prepared by <Ayman Makhoukhi, OICF7P> for the DDS Distributed Reatke.
%% I declare that this solution is my work.
%% I have not copied or used third party solutions.
%% I have not passed my solution to my classmates, nor made it public.
%% Students’ regulation of Eötvös Loránd University (ELTE Regulations Vol. II. 74/C. § ) states that as long as a student presents another student’s work - or at least the significant part of it - as his/her performance, it will count as a disciplinary fault. The most serious consequence of a disciplinary fault can be the dismissal of the student from the University.

% cd("C:/Users/oicf7p/Desktop").
% laundry@loviw036
% erl.exe -sname laundry

-module(laundry1).
-compile(export_all).
-define(LD, {server,'laundry@ayman-ZenBook-UX363JA-UX363JA'}).

start(Price,Program)->
    register(server,spawn_link(?MODULE, loop,[Price,Program,0])).


loop(Price,Program,MoneyInserted)->
    receive
        {send_money, Pid, Money}->
            case Money < Price of
                false -> Pid ! {program_option, Program};
                true -> Pid ! insufficient_funds
            end,
            loop(Price,Program,Money);

        {select_program,Pid, ProgramOption} ->
            {ProgramEl, _Type} = ProgramOption,
            RestOfProgram = lists:keydelete(ProgramEl, 1, Program),
            Pid ! {next,RestOfProgram},
            loop(Price,RestOfProgram, MoneyInserted);

        %{select_program,Pid, ProgramOption} ->
        %    {ProgramEl, _Type} = ProgramOption,
        %        case list:keyfind(ProgramEl,1, Program) of
        %            false -> {money_returned, MoneyInserted};
        %            _ -> RestOfProgram = lists:keydelete(ProgramEl, 1, Program),
        %                 Pid ! {next,RestOfProgram},
        %                 loop(Price,RestOfProgram, MoneyInserted)
        %        end;

        {start_machine,Pid}-> 
            Pid ! {started,{returned_money, MoneyInserted - Price}},
            loop(Price,Program, MoneyInserted)
    end.

insert_coins(Money)->
    ?LD ! {send_money, self(),Money},
    receive
        {program_option, Program}-> Program;
        insufficient_funds -> insufficient_funds
    end.

select_program(start)->
     ?LD ! {start_machine,self()},
     receive
        MSG -> MSG
     end;

select_program(ProgramOption)->
    ?LD ! {select_program,self(),ProgramOption},
    receive
        {next, Rest} -> {next, Rest}
    end.
