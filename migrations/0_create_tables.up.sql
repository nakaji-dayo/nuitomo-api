-- Entities

create table "user" (
  id bigint not null,
  primary key (id)
);

create table task (
  id bigint not null,
  "name" text not null,
  user_id bigint references "user"(id) not null,
  primary key (id)
);

create table task_tag (
  id bigint not null,
  task_id bigint references "task"(id) not null,
  "name" text not null,
  primary key (id)
);