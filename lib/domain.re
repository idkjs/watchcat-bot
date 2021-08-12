module IntMap = Map.Make(Int);

[@deriving compare]
type user_key = {
  chat_id: Int.t,
  user_id: Int.t,
};

type user_info = {name: string};

module UserMap =
  Map.Make({
    [@deriving compare]
    type t = user_key;
  });

type state = {trusted_users: UserMap.t(user_info)};

module StateEvents = {
  [@deriving yojson]
  type event =
    | TrustedUserAdded({
        chat_id: int,
        user_id: int,
        name: string,
      })
    | TrustedUserDeleted({
        chat_id: int,
        user_id: int,
      });

  let empty_state = {trusted_users: UserMap.empty};

  let restore = state =>
    fun
    | TrustedUserAdded({chat_id, user_id, name}) => {
        trusted_users:
          UserMap.add(
            {chat_id, user_id},
            {name: name},
            state.trusted_users,
          ),
      }
    | TrustedUserDeleted({chat_id, user_id}) => {
        trusted_users:
          UserMap.remove({chat_id, user_id}, state.trusted_users),
      };
};

let new_chat_member = (_env, _user_id) => [];

let find_user_in_message' = entities =>
  TelegramApi.MessageEntity.(
    entities
    |> Option.fold(~none=[], ~some=Fun.id)
    |> List.find_opt(x =>
         switch (x.entity_type) {
         | TextMention(_) => true
         | _ => false
         }
       )
    |> (
      fun
      | Some({entity_type: TextMention(user), _}) => Some(user)
      | _ => None
    )
  );

open TelegramApi.Message;

let find_user_in_message = msg =>
  switch (msg) {
  | {reply_to_message: Some({from: Some(user), _}), _} => Some(user)
  | _ => find_user_in_message'(msg.entities)
  };

let user_to_string = ({TelegramApi.User.first_name, username, _}) =>
  username
  |> Option.fold(~none="", ~some=un => " (@" ++ un ++ ")")
  |> Printf.sprintf("%s%s", first_name);

let add_trusted_user' = ({chat: {id: chat_id, _}, _} as msg) =>
  switch (find_user_in_message(msg)) {
  | Some(trusted_user) =>
    let tu_title = user_to_string(trusted_user);
    [
      `UpdateState([
        StateEvents.TrustedUserAdded({
          chat_id,
          user_id: trusted_user.id,
          name: tu_title,
        }),
      ]),
    ];
  | None => [`SendMessage("Пользователь не указан")]
  };

let remove_trusted_user' = ({chat: {id: chat_id, _}, _} as msg) =>
  switch (find_user_in_message(msg)) {
  | Some(trusted_user) => [
      `UpdateState([
        StateEvents.TrustedUserDeleted({chat_id, user_id: trusted_user.id}),
      ]),
    ]
  | None => [`SendMessage("Пользователь не указан")]
  };

let try_ban = (env, msg) => {
  let is_spam = reply_msg => {
    let jc_regex = Str.regexp("https://t\\.me/joinchat/.+");
    let bit_regex = Str.regexp(".*https://bit\\.ly/.+");
    Option.is_none(reply_msg.text)
    && Option.is_some(reply_msg.photo)
    || Str.string_match(
         jc_regex,
         reply_msg.text |> Option.fold(~none="", ~some=Fun.id),
         0,
       )
    || Str.string_match(
         bit_regex,
         reply_msg.text |> Option.fold(~none="", ~some=Fun.id),
         0,
       );
  };

  switch (msg) {
  | {
      from: Some({id: user_id, _}),
      reply_to_message:
        Some({from: Some({id: spam_user_id, _}), _} as spam_msg),
      _,
    } =>
    let state = env#state
    and delete_spam = [
      `DeleteMessage(spam_msg.message_id),
      `KickUser(spam_user_id),
      `DeleteMessage(msg.message_id),
    ];

    if (env#is_admin) {
      delete_spam;
    } else if (UserMap.mem(
                 {chat_id: msg.chat.id, user_id},
                 state.trusted_users,
               )
               && msg.date
               - spam_msg.date <= 90
               && is_spam(spam_msg)) {
      delete_spam;
    } else {
      [`DeleteMessage(msg.message_id)];
    };
  | _ => [`DeleteMessage(msg.message_id)]
  };
};

type user_command('env, 'a) = {
  name: string,
  description: string,
  run: ('env, message) => 'a,
};

let only_admin = (f, env, msg) => env#is_admin ? f(msg) : [];

let delete_message = (f, msg) => [
  `DeleteMessage(msg.message_id),
  ...f(msg),
];

let add_trusted_user = msg =>
  delete_message(only_admin(add_trusted_user', msg));

let remove_trusted_user = msg =>
  delete_message(only_admin(remove_trusted_user', msg));

let user_commands = [
  {
    name: "ban",
    description: "Забанить пользователя",
    run: try_ban,
  },
  {
    name: "baka",
    description: "Забанить пользователя (синоним ban)",
    run: try_ban,
  },
  {
    name: "add",
    description: "Добавить доверенного пользователя",
    run: add_trusted_user,
  },
  {
    name: "remove",
    description: "Удалить доверенного пользователя",
    run: remove_trusted_user,
  },
  {name: "version", description: "Версия 0.2", run: (_, _) => []},
];
