create table owner_key (
  id bigint not null,
  owner_id text not null,
  key text not null,
  unique (key),
  primary key (id)
);

ALTER TABLE owner_user ADD CONSTRAINT owner_user_user_id_owner_id_key UNIQUE (user_id, owner_id);
