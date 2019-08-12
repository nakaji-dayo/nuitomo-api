create table owner_token (
  id bigint not null,
  owner_id text not null,
  token text not null,
  unique (owner_id, token),
  primary key (id)
);
