create table "user" (
  id bigint not null,
  name text not null,
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
