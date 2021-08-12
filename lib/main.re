open Domain;
open Telegram.Api;

module MakeWatchcatBot = (Env: {let storage_path: string;}) =>
  Mk({
    include Telegram.BotDefaults;
    open Command;
    open Message;

    let init_events =
      Persistent.load_events(
        Domain.StateEvents.event_of_yojson,
        Env.storage_path,
      );

    let state_store =
      ref(
        Persistent.restore_from_events(
          Domain.StateEvents.restore,
          Domain.StateEvents.empty_state,
          init_events,
        ),
      );

    let save_to_disk =
      Persistent.save_to_disk(
        init_events,
        Env.storage_path,
        StateEvents.event_to_yojson,
      );

    let handle_effects = (chat_id, effects) => {
      open Telegram.Actions;
      let handleEffect = (chat_id, effect) =>
        switch (effect) {
        | `DeleteMessage(message_id) => delete_message(~chat_id, ~message_id)
        | `KickUser(user_id) => kick_chat_member(~chat_id, ~user_id)
        | `UpdateState(events) =>
          state_store :=
            events |> List.fold_left(StateEvents.restore, state_store^);
          save_to_disk(events);
          nothing;
        | `SendMessage(message) =>
          send_message(~chat_id, ~disable_notification=true, "%s", message)
        | `None => nothing
        };

      effects |> List.map(handleEffect(chat_id)) |> sequence;
    };

    let token = Sys.getenv("TELEGRAM_TOKEN");

    let command_postfix = Some("watchcat_y2k_bot");

    let make_env = (is_admin: bool, user: option(User.user)) => {
      as _;
      pub user = user;
      pub is_admin = is_admin;
      pub state = state_store^
    };

    let new_chat_member = (chat: Chat.chat, user: User.user) =>
      Domain.new_chat_member(make_env(false, Some(user)), user.id)
      |> handle_effects(chat.id);

    let is_admin = (f, msg) =>
      Telegram.Actions.(
        (
          switch (msg) {
          | {chat: {id: chat_id, _}, from: Some({id: user_id, _}), _} =>
            let is_member =
              ChatMember.(
                List.exists(({user: member, _}) => user_id == member.id)
              );

            get_chat_administrators(
              ~chat_id,
              ~and_then=
                fun
                | Result.Success(members) when is_member(members) =>
                  f(msg, true)
                | _ => f(msg, false),
            );
          | _ => nothing
          }
        )
        |> Lwt.return
      );

    let last_json = ref("");

    let hook_update = (json, data) => {
      last_json := json;
      data;
    };

    let commands = {
      let wrap = f =>
        is_admin((msg, is_admin) =>{
          let env = make_env(is_admin, msg.from);
          let effs = f(env, msg);
          let reply_text = Option.bind(msg.reply_to_message, x => x.text);
          Logger.log(env, last_json^, reply_text, effs);
          effs |> handle_effects(msg.chat.id);
        });

      Domain.user_commands
      |> List.map((uc: Domain.user_command(_)) =>
           {
             name: uc.name,
             description: uc.description,
             enabled: true,
             run: wrap(uc.run),
           }
         );
    };
  });

let run = storage_path => {
  module WatchcatBot =
    MakeWatchcatBot({
      let storage_path = storage_path;
    });
  print_endline("Bot started...");
  WatchcatBot.run(~log=true, ());
};
