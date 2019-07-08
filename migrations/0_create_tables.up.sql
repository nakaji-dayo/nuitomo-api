create table "user" (
  id bigint not null,
  name text not null,
  bio text not null,
  --
  nickname text not null,
  gender text not null,
  hometown text not null,
  entry_date text not null,
  favorite_thing text not null,
  dislike_thing text not null,
  primary key (id)
);

create table owner_user (
  id bigint not null,
  owner_id text not null,
  user_id bigint references "user"(id) not null,
  is_primary boolean not null,
  primary key (id)
);

create table user_image (
  id bigint not null,
  user_id bigint references "user"(id) not null,
  url text not null,
  primary key (id)
);

create table post (
  id bigint not null,
  user_id bigint references "user"(id) not null,
  body text not null,
  reply_to bigint references "post"(id),
  mention_to bigint references "user"(id), -- 当面はbotのみが利用
  primary key (id)
);

create table post_image (
  id bigint not null,
  post_id bigint references "post"(id) not null,
  url text not null,
  primary key (id)
);

create table follow (
  id bigint not null,
  user_id bigint references "user"(id) not null,
  to_user_id bigint references "user"(id) not null,
  unique (user_id, to_user_id),
  primary key (id)
);

create table "like" (
  id bigint not null,
  post_id bigint references "post"(id) not null,
  user_id bigint references "user"(id) not null,
  unique (post_id, user_id),
  primary key (id)
);

create table report (
  id bigint not null,
  category smallint not null,
  user_id bigint references "user"(id) not null,
  target_user_id bigint references "user"(id) not null,
  primary key (id)
);

create table notification (

);
